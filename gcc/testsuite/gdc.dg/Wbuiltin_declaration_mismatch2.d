// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
module gcc.simd;

alias int4 = __vector(int[4]);
alias short8 = __vector(short[8]);
alias float4 = __vector(float[4]);
alias byte16 = __vector(byte[16]);
struct fake4 { int[4] v; }
enum f = fake4();

void test_load_store()
{
    loadUnaligned!int(null); // { dg-warning "mismatch in return type" }
    loadUnaligned!double(null); // { dg-warning "mismatch in return type" }
    loadUnaligned!int4(null);
    loadUnaligned!short8(null);
    loadUnaligned!float4(null);
    loadUnaligned!byte16(null);
    loadUnaligned!fake4(null); // { dg-warning "mismatch in return type" }

    storeUnaligned!int(null, 1); // { dg-warning "mismatch in return type" }
    storeUnaligned!double(null, 1); // { dg-warning "mismatch in return type" }
    storeUnaligned!int4(null, 1);
    storeUnaligned!short8(null, 1);
    storeUnaligned!float4(null, 1);
    storeUnaligned!byte16(null, 1);
    storeUnaligned!fake4(null, f); // { dg-warning "mismatch in return type" }
}

// The following declarations of the simd intrinsics are without any guards
// to verify `d/intrinsics.cc` is doing checks to prevent invalid lowerings.
V loadUnaligned(V)(const V*);
V storeUnaligned(V)(V*, V);
