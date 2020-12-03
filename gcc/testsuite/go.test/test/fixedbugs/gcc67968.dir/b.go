package b

import "./a"

func F() (interface{}) {
     var v *a.T
     return v.Foo()
}
