// printnil checks that fmt correctly handles a nil pointer receiver
// for a value method at all optimization levels.
package main

import "fmt"

type MyType struct {
	val int
}

func (t MyType) String() string {
	return "foobar"
}

func main() {
	if got := fmt.Sprintf("%s", (*MyType)(nil)); got != "<nil>" {
		panic(got)
	}
}
