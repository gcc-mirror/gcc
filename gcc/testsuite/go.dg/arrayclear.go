// { dg-do compile }
// { dg-options "-fgo-debug-optimization" }
// This comment is necessary to work around a dejagnu bug. Otherwise, the
// column of the second error message would equal the row of the first one, and
// since the errors are also identical, dejagnu is not able to distinguish them.

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
