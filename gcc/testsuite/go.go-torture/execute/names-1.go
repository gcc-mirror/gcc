// names-1 is a change detector for Go symbol names.  We don't want
// the name mangling to change silently.
package main

import (
	"bytes"
	"debug/elf"
	"debug/macho"
	"debug/pe"
	"fmt"
	"os"
	"runtime"
	"strings"
)

type Type int
type Alias = int

//go:noinline
func Function1(out *bytes.Buffer) int {
	var f2 func(int) int
	f1 := func(i int) int {
		if i == 0 {
			return 0
		}
		type NestedType struct { a int }
		t := NestedType{f2(i-1)}
		fmt.Fprint(out, t)
		return t.a
	}
	f2 = func(i int) int {
		if i == 0 {
			return 0
		}
		type NestedType struct { a int }
		t := NestedType{f1(i-1)}
		fmt.Fprint(out, t)
		return t.a
	}
	return f1(10) + f2(10)
}

//go:noinline
func Function2(out *bytes.Buffer) {
	{
		type T struct { b int }
		fmt.Fprint(out, T{1})
	}
	{
		type T struct { b int }
		fmt.Fprint(out, T{2})
	}
}

func (t Type) M(bool, int8, float32, complex64, string, func(), func(int16) (float64, complex128), *byte, struct { f int "tag #$%^&{}: 世界" }, []int32, [24]int64, map[uint8]uint16, chan uint32, <-chan uint64, chan <- uintptr, Type, Alias) {
}

//go:noinline
func Function3(out *bytes.Buffer) {
	fmt.Fprintf(out, "%T", Type(0))
}

func main() {
	if runtime.GOOS == "aix" {
		// Not supported on AIX until there is an externally
		// visible version of internal/xcoff.
		return
	}

	var b bytes.Buffer
	Function1(&b)
	Function2(&b)
	Function3(&b)
	_ = len(b.String())

	for _, n := range []string{"/proc/self/exe", os.Args[0]} {
		if f, err := os.Open(n); err == nil {
			checkFile(f)
			return
		}
	}
	fmt.Println("checksyms: could not find executable")
	fmt.Println("UNSUPPORTED: checksyms")
}

func checkFile(f *os.File) {
	var syms []string
	if ef, err := elf.NewFile(f); err == nil {
		esyms, err := ef.Symbols()
		if err != nil {
			panic(err)
		}
		for _, esym := range esyms {
			syms = append(syms, esym.Name)
		}
	} else if mf, err := macho.NewFile(f); err == nil {
		for _, msym := range mf.Symtab.Syms {
			syms = append(syms, msym.Name)
		}
	} else if pf, err := pe.NewFile(f); err == nil {
		for _, psym := range pf.Symbols {
			syms = append(syms, psym.Name)
		}
	} else {
		fmt.Println("checksyms: could not parse executable")
		fmt.Println("UNSUPPORTED: checksyms")
		return
	}
	checkSyms(syms)
}

var want = []string{
	"main.Function1",
	"main.Function1..f",
	"main.Function1..func1",
	"main.Function1..func1.main.NestedType..d",
	"main.Function1..func2",
	"main.Function1..func2.main.NestedType..d",
	"main.Function2",
	"main.Function2..f",
	"main.Function2.main.T..d",
	"main.Function2.main.T..i1..d",
	"main.Function3",
	"main.Function3..f",
	"main.Type..d",
	"main.Type.M",
	"main.main",
	"main.want",
	"type...1.1main.Type",  // Why is this here?
	"type...1main.Function1..func1.NestedType",
	"type...1main.Function1..func2.NestedType",
	"type...1main.Function2.T",
	"type...1main.Function2.T..i1",
	"type...1main.Type",
	"type..func.8.1main.Type.3bool.3int8.3float32.3complex64.3string.3func.8.9.8.9.3func.8int16.9.8float64.3complex128.9.3.1uint8.3struct.4.main.f.0int.4tag.x20.x23.x24.x25.x5e.x26.x7b.x7d.x3a.x20..u4e16..u754c.5.5.3.6.7int32.3.624.7int64.3map.6uint8.7uint16.3chan.0uint32.3.4.5chan.0uint64.3chan.4.5.0uintptr.3main.Type.3int.9.8.9",
	"type..func.8bool.3int8.3float32.3complex64.3string.3func.8.9.8.9.3func.8int16.9.8float64.3complex128.9.3.1uint8.3struct.4.main.f.0int.4tag.x20.x23.x24.x25.x5e.x26.x7b.x7d.x3a.x20..u4e16..u754c.5.5.3.6.7int32.3.624.7int64.3map.6uint8.7uint16.3chan.0uint32.3.4.5chan.0uint64.3chan.4.5.0uintptr.3main.Type.3int.9.8.9",
	"type..func.8main.Type.3bool.3int8.3float32.3complex64.3string.3func.8.9.8.9.3func.8int16.9.8float64.3complex128.9.3.1uint8.3struct.4.main.f.0int.4tag.x20.x23.x24.x25.x5e.x26.x7b.x7d.x3a.x20..u4e16..u754c.5.5.3.6.7int32.3.624.7int64.3map.6uint8.7uint16.3chan.0uint32.3.4.5chan.0uint64.3chan.4.5.0uintptr.3main.Type.3int.9.8.9",
	"type..struct.4.main.f.0int.4tag.x20.x23.x24.x25.x5e.x26.x7b.x7d.x3a.x20..u4e16..u754c.5.5",
}

func checkSyms(syms []string) {
	m := make(map[string]bool)
	for _, sym := range syms {
		if strings.Contains(sym, ".") {
			m[sym] = true
		}
	}

	ok := true
	for _, w := range want {
		if m[w] {
			delete(m, w)
		} else {
			fmt.Printf("checksyms: missing expected symbol %q\n", w)
			ok = false
		}
	}

	for sym := range m {
		if !strings.Contains(sym, "main") {
			continue
		}

		// Skip some symbols we may see but know are unimportant.
		if sym == "go-main.c" {
			continue
		}
		if strings.HasPrefix(sym, "runtime.") {
			continue
		}

		// We can see a lot of spurious .eq and .hash
		// functions for types defined in other packages.
		// This is a bug but skip them for now.
		if strings.Contains(sym, "..eq") || strings.Contains(sym, "..hash") {
			continue
		}

		// Skip closure types by skipping incomparable structs.
		// This may be a bug, not sure.
		if strings.Contains(sym, ".4x.5") {
			continue
		}

		// These functions may be inlined.
		if sym == "main.checkFile" || sym == "main.checkSyms" {
			continue
		}

		fmt.Printf("checksyms: found unexpected symbol %q\n", sym)
		ok = false
	}

	if !ok  {
		fmt.Println("FAIL: checksyms")
	}
}
