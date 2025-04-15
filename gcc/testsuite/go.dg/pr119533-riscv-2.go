// { dg-do compile { target riscv64*-*-* } }
// { dg-options "-O2 -march=rv64gcv -mabi=lp64d" }

package ast

type as struct {
	bt []struct{}
	an string
}

func bj(a *as) string {
	if b := a.bt; len(a.an) == 1 {
		_ = b[0]
	}
	return a.an
}

func MergePackageFiles(f map[string][]interface{}, g uint) []interface{} {
	bl := make([]string, len(f))
	var bo []interface{}
	bu := make(map[string]int)
	for _, bm := range bl {
		a := f[bm]
		for _, d := range a {
			if g != 0 {
				if a, p := d.(*as); p {
					n := bj(a)
					if j, bp := bu[n]; bp {
						_ = j
					}
				}
			}
		}
	}
	for _, bm := range bl {
		_ = bm
	}
	for _, bm := range bl {
		_ = f[bm]
	}
	return bo
}
