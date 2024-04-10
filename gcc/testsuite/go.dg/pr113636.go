// { dg-do compile }
// { dg-options "-O3 -g" }
// { dg-additional-options "-mtune=thunderxt88" { target aarch64*-*-* } }

package main

import "math"

func sinhcosh(x float64) (sh, ch float64) {
	if math.Abs(x) <= 0.5 {
		return math.Sinh(x), math.Cosh(x)
	}
	e := math.Exp(x)
	ei := 0.5 / e
	e *= 0.5
	return e - ei, e + ei
}

func Cos(x complex128) complex128 {
	switch re, im := real(x), imag(x); {
	case im == 0 && (math.IsInf(re, 0) || math.IsNaN(re)):
		return complex(math.NaN(), -im*math.Copysign(0, re))
	case math.IsInf(im, 0):
		switch {
		// case re == 0:
		// 	return complex(math.Inf(1), -re*math.Copysign(0, im))
		case math.IsInf(re, 0) || math.IsNaN(re):
			return complex(math.Inf(1), math.NaN())
		}
	// case re == 0 && math.IsNaN(im):
	// 	return complex(math.NaN(), 0)
	}
	s, c := math.Sincos(real(x))
	sh, ch := sinhcosh(imag(x))
	return complex(c*ch, -s*sh)
}

func main() {
	Cos(complex(2.5, 3.5))
}
