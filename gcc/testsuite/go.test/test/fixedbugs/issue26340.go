// compile

// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// gccgo did not permit omitting the type of a composite literal
// element when the element type is a pointer type.

package p

type S []T
type T struct { x int }

var _ = map[string]*S{
	"a": {
		{ 1 },
	},
}

var _ = [1]*S{ { {1}, } }
