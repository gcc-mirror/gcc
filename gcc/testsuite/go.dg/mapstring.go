// { dg-do compile }
// { dg-options "-fgo-debug-optimization" }

package p

func F(m map[string]int, a, b []byte) int {
	x := m[string(a)]     // { dg-error "no copy string\\(\\\[\\\]byte\\)" }
	y, ok := m[string(b)] // { dg-error "no copy string\\(\\\[\\\]byte\\)" }
	_ = ok
	return x + y
}
