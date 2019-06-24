// { dg-do compile }
// { dg-options "-fgo-debug-optimization" }

package p

func F(b []byte, x string) string {
	return "hello " + string(b) + x // { dg-error "no copy string\\(\\\[\\\]byte\\)" }
}
