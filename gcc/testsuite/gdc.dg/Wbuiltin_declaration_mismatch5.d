// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
module gcc.simd;

alias int4 = __vector(int[4]);
alias short8 = __vector(short[8]);
alias float4 = __vector(float[4]);
alias byte16 = __vector(byte[16]);
struct fake4 { int[4] v; }
enum f = fake4();

void test_convertvector()
{
    convertvector!(int, int)(0); // { dg-warning "mismatch in return type" }
    convertvector!(double, int)(0); // { dg-warning "mismatch in return type" }
    convertvector!(fake4, int)(0); // { dg-warning "mismatch in return type" }

    convertvector!(int4, int)(0); // { dg-warning "mismatch in argument 1" }
    convertvector!(int4, double)(0); // { dg-warning "mismatch in argument 1" }
    convertvector!(int4, int4)(0);
    convertvector!(int4, short8)(0); // { dg-error "mismatch in argument 1" }
    convertvector!(int4, float4)(0);
    convertvector!(int4, byte16)(0); // { dg-error "mismatch in argument 1" }
    convertvector!(int4, fake4)(f); // { dg-warning "mismatch in argument 1" }

    convertvector!(short8, int)(0); // { dg-warning "mismatch in argument 1" }
    convertvector!(short8, double)(0); // { dg-warning "mismatch in argument 1" }
    convertvector!(short8, int4)(0); // { dg-error "mismatch in argument 1" }
    convertvector!(short8, short8)(0);
    convertvector!(short8, float4)(0); // { dg-error "mismatch in argument 1" }
    convertvector!(short8, byte16)(0); // { dg-error "mismatch in argument 1" }
    convertvector!(short8, fake4)(f); // { dg-warning "mismatch in argument 1" }

    convertvector!(float4, int)(0); // { dg-warning "mismatch in argument 1" }
    convertvector!(float4, double)(0); // { dg-warning "mismatch in argument 1" }
    convertvector!(float4, int4)(0);
    convertvector!(float4, short8)(0); // { dg-error "mismatch in argument 1" }
    convertvector!(float4, float4)(0);
    convertvector!(float4, byte16)(0); // { dg-error "mismatch in argument 1" }
    convertvector!(float4, fake4)(f); // { dg-warning "mismatch in argument 1" }

    convertvector!(byte16, int)(0); // { dg-warning "mismatch in argument 1" }
    convertvector!(byte16, double)(0); // { dg-warning "mismatch in argument 1" }
    convertvector!(byte16, int4)(0); // { dg-error "mismatch in argument 1" }
    convertvector!(byte16, short8)(0); // { dg-error "mismatch in argument 1" }
    convertvector!(byte16, float4)(0); // { dg-error "mismatch in argument 1" }
    convertvector!(byte16, byte16)(0);
    convertvector!(byte16, fake4)(f); // { dg-warning "mismatch in argument 1" }
}

// The following declarations of the simd intrinsics are without any guards
// to verify `d/intrinsics.cc` is doing checks to prevent invalid lowerings.
V convertvector(V, T)(T);
