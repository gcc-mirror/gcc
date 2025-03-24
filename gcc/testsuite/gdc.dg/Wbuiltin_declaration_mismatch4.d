// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
module gcc.simd;

alias int4 = __vector(int[4]);
alias short8 = __vector(short[8]);
alias float4 = __vector(float[4]);
alias byte16 = __vector(byte[16]);
struct fake4 { int[4] v; }
enum f = fake4();

void test_shufflevector()
{
    shufflevector!(int, int4, int, int, int, int)(0, 0, 0, 0, 0, 0); // { dg-warning "mismatch in argument 1" }
    shufflevector!(double, int4, int, int, int, int)(0, 0, 0, 0, 0, 0); // { dg-warning "mismatch in argument 1" }
    shufflevector!(fake4, int4, int, int, int, int)(f, 0, 0, 0, 0, 0); // { dg-warning "mismatch in argument 1" }

    shufflevector!(int4, int, int, int, int, int)(0, 0, 0, 0, 0, 0); // { dg-warning "mismatch in argument 2" }
    shufflevector!(int4, double, int, int, int, int)(0, 0, 0, 0, 0, 0); // { dg-warning "mismatch in argument 2" }
    shufflevector!(int4, int4, int, int, int, int)(0, 0, 0, 0, 0, 0);
    shufflevector!(int4, short8, int, int, int, int)(0, 0, 0, 0, 0, 0); // { dg-error "mismatch in argument 2" }
    shufflevector!(int4, float4, int, int, int, int)(0, 0, 0, 0, 0, 0); // { dg-error "mismatch in argument 2" }
    shufflevector!(int4, byte16, int, int, int, int)(0, 0, 0, 0, 0, 0); // { dg-error "mismatch in argument 2" }
    shufflevector!(int4, fake4, int, int, int, int)(0, f, 0, 0, 0, 0); // { dg-warning "mismatch in argument 2" }

    shufflevector!(int4, int4, double, int, int, int)(0, 0, 0, 0, 0, 0); // { dg-warning "mismatch in argument 3" }
    shufflevector!(int4, int4, int4, int, int, int)(0, 0, 0, 0, 0, 0); // { dg-warning "mismatch in argument 3" }
    shufflevector!(int4, int4, short8, int, int, int)(0, 0, 0, 0, 0, 0); // { dg-warning "mismatch in argument 3" }
    shufflevector!(int4, int4, float4, int, int, int)(0, 0, 0, 0, 0, 0); // { dg-warning "mismatch in argument 3" }
    shufflevector!(int4, int4, byte16, int, int, int)(0, 0, 0, 0, 0, 0); // { dg-warning "mismatch in argument 3" }

    shufflevector!(int4, int4, int, double, int, int)(0, 0, 0, 0, 0, 0); // { dg-warning "mismatch in argument 4" }
    shufflevector!(int4, int4, int, int, double, int)(0, 0, 0, 0, 0, 0); // { dg-warning "mismatch in argument 5" }
    shufflevector!(int4, int4, int, int, int, double)(0, 0, 0, 0, 0, 0); // { dg-warning "mismatch in argument 6" }

    int i;
    shufflevector!(int4, int4, int, int, int, int)(0, 0, i, 0, 0, 0); // { dg-error "argument .i. cannot be read at compile time" }
    shufflevector!(int4, int4, int, int, int, int)(0, 0, -1u, 0, 0, 0); // { dg-error "element index .-1. is out of bounds" }
    shufflevector!(int4, int4, int, int, int, int)(0, 0, 8, 0, 0, 0); // { dg-error "element index .8. is out of bounds" }
}

// The following declarations of the simd intrinsics are without any guards
// to verify `d/intrinsics.cc` is doing checks to prevent invalid lowerings.

// Use overloads to test different argument positions.
template E(V) { alias typeof(V.array[0]) E; }
enum isV(T) = is(T : __vector(V[N]), V, size_t N);

__vector(E!V1[M.length]) shufflevector(V1, V2, M...)(V1, V2, M) if (isV!V1 && !isV!V2);
__vector(E!V2[M.length]) shufflevector(V1, V2, M...)(V1, V2, M) if (isV!V2 && !isV!V1);
__vector(E!V1[M.length]) shufflevector(V1, V2, M...)(V1, V2, M) if (isV!V1 && isV!V2);
