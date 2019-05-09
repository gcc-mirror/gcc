// { dg-do compile }
// { dg-options "-fgo-debug-optimization" }

package p

func F(x []byte, y string) bool {
	return string(x) == y // { dg-error "no copy string\\(\\\[\\\]byte\\)" }
}

func BytesEqual(x, y []byte) bool {
	return string(x) == // { dg-error "no copy string\\(\\\[\\\]byte\\)" }
		string(y)   // { dg-error "no copy string\\(\\\[\\\]byte\\)" }
}
