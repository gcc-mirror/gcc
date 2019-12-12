// { dg-do compile }
// { dg-options "-fgo-debug-optimization" }

package p

var a [10]int

func arrayClear() {
	for i := range a { // { dg-error "array range clear" }
		a[i] = 0
	}
}

var s []int

func sliceClear() {
	for i := range s { // { dg-error "array range clear" }
		s[i] = 0
	}
}
