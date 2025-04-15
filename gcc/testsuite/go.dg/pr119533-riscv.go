// { dg-do compile { target riscv64*-*-* } }
// { dg-options "-O2 -march=rv64gcv -mabi=lp64d" }

// Reduced from libgo build (multi-file reduction, merged mnaully
// and hand reduced again).

package ast
import (
	"go/token"
	"go/scanner"
	"reflect"
)
type v struct {}
type w func( string,  reflect.Value) bool
func x( string,  reflect.Value) bool
type r struct {
	scanner.ErrorList
}
type ab interface {}
type ae interface {}
type af interface {}
type ag struct {}
func (ag) Pos() token.Pos
func (ag) ah() token.Pos
type c struct {
	aj    ae          }
type ak struct {
	al    []c  }
type (
	am struct {
		an    string    }
	bs struct {
		Value    string
	  }
)
func ao(string) *am
type (
	ap interface {}
	aq struct {
		ar    bs     }
as struct {
		bt ak
		an am        }
)
type File struct {
	*ag
	token.Pos
	*am
	at      []af
	*v
	au    []*aq
	av *am
	aw   []*ag }
type ax struct {
	an    string
	*v
	ay   map[string]File   }
func a(az *token.FileSet, b token.Pos) int
type k struct {
	l token.Pos
	ah   token.Pos
}
type m struct {
	bb bool
	bc   *ag
}

type bi uint
func bj(a *as) string {
	if b := a.bt; len(b.al) == 1 {
		c := b.al[0].aj
		if e := c; e != nil {}
	}
	return a.an.an
}
func MergePackageFiles(f ax, g bi) *File {
	h := 0
	bk := 0
	k := 0
	bl := make([]string, len(f.ay))
	i := 0
	for bm, a := range f.ay {
		bl[i] = bm
		k += len(a.at)
	}
	var bn *ag
	var l token.Pos
	if h > 0 {}
	var bo []af
		bu := make(map[string]int)
		m := 0
		for _, bm := range bl {
			a := f.ay[bm]
			for _, d := range a.at {
				if g!= 0 {
					if a, p := d.(*as); p {
						n := bj(a)
						if j, bp := bu[n]; bp {
							if bo != nil && bo[j]== nil {}
						}
					}
				}
			}
		}
		if m > 0 {}
	var bq []*aq
		q := make(map[string]bool)
		for _, bm := range bl {
			a := f.ay[bm]
			for _, br := range a.au {
				if o := br.ar.Value; q[o] {}
			}
		}
	var bh = make([]*ag, bk)
		for _, bm := range bl {
			a := f.ay[bm]
			copy(bh, a.aw)
		}
	return &File{bn, l, ao(f.an), bo, f.v, bq, nil, bh}
}
