// { dg-do compile }
// { dg-options "-fgo-debug-optimization" }

package p

func clear(m map[int]int) {
	for k := range m { // { dg-error "map range clear" }
		delete(m, k)
	}
}
