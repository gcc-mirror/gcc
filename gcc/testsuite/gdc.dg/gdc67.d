// https://bugzilla.gdcproject.org/show_bug.cgi?id=67
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }

__vector(float[4])[2] d;  // ICE
