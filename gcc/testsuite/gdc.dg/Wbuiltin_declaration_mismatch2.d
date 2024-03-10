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

void test_shuffle()
{
    shuffle!(int, int, int)(0, 0, 0); // { dg-warning "mismatch in return type" }
    shuffle!(double, int, int)(0, 0, 0); // { dg-warning "mismatch in return type" }
    shuffle!(fake4, int, int)(f, 0, 0); // { dg-warning "mismatch in return type" }

    shuffle!(int4, int, int)(0, 0, 0); // { dg-warning "mismatch in argument 2" }
    shuffle!(int4, double, int)(0, 0, 0); // { dg-warning "mismatch in argument 2" }
    shuffle!(int4, fake4, int)(0, f, 0); // { dg-warning "mismatch in argument 2" }

    shuffle!(int4, int4, int)(0, 0, 0); // { dg-warning "mismatch in argument 3" }
    shuffle!(int4, int4, double)(0, 0, 0); // { dg-warning "mismatch in argument 3" }
    shuffle!(int4, int4, fake4)(0, 0, f); // { dg-warning "mismatch in argument 3" }

    shuffle!(int4, int4, int4)(0, 0, 0);
    shuffle!(int4, short8, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    shuffle!(int4, float4, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    shuffle!(int4, byte16, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    shuffle!(int4, int4, short8)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    shuffle!(int4, int4, float4)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    shuffle!(int4, int4, byte16)(0, 0, 0); // { dg-error "mismatch in argument 3" }

    shuffle!(float4, int4, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    shuffle!(float4, short8, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    shuffle!(float4, float4, int4)(0, 0, 0);
    shuffle!(float4, byte16, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    shuffle!(float4, float4, short8)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    shuffle!(float4, float4, float4)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    shuffle!(float4, float4, byte16)(0, 0, 0); // { dg-error "mismatch in argument 3" }

    shuffle!(short8, int4, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    shuffle!(short8, short8, int4)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    shuffle!(short8, float4, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    shuffle!(short8, byte16, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    shuffle!(short8, short8, short8)(0, 0, 0);
    shuffle!(short8, short8, float4)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    shuffle!(short8, short8, byte16)(0, 0, 0); // { dg-error "mismatch in argument 3" }

    shuffle!(byte16, int4, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    shuffle!(byte16, short8, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    shuffle!(byte16, float4, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    shuffle!(byte16, byte16, int4)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    shuffle!(byte16, byte16, short8)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    shuffle!(byte16, byte16, float4)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    shuffle!(byte16, byte16, byte16)(0, 0, 0);
}

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

void test_blendvector()
{
    blendvector!(int, int, int)(0, 0, 0); // { dg-warning "mismatch in return type" }
    blendvector!(double, int, int)(0, 0, 0); // { dg-warning "mismatch in return type" }
    blendvector!(fake4, int, int)(f, 0, 0); // { dg-warning "mismatch in return type" }

    blendvector!(int4, int, int)(0, 0, 0); // { dg-warning "mismatch in argument 2" }
    blendvector!(int4, double, int)(0, 0, 0); // { dg-warning "mismatch in argument 2" }
    blendvector!(int4, fake4, int)(0, f, 0); // { dg-warning "mismatch in argument 2" }

    blendvector!(int4, int4, int)(0, 0, 0); // { dg-warning "mismatch in argument 3" }
    blendvector!(int4, int4, double)(0, 0, 0); // { dg-warning "mismatch in argument 3" }
    blendvector!(int4, int4, fake4)(0, 0, f); // { dg-warning "mismatch in argument 3" }

    blendvector!(int4, int4, int4)(0, 0, 0);
    blendvector!(int4, short8, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(int4, float4, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(int4, byte16, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(int4, int4, short8)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    blendvector!(int4, int4, float4)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    blendvector!(int4, int4, byte16)(0, 0, 0); // { dg-error "mismatch in argument 3" }

    blendvector!(float4, int4, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(float4, short8, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(float4, float4, int4)(0, 0, 0);
    blendvector!(float4, byte16, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(float4, float4, short8)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    blendvector!(float4, float4, float4)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    blendvector!(float4, float4, byte16)(0, 0, 0); // { dg-error "mismatch in argument 3" }

    blendvector!(short8, int4, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(short8, short8, int4)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    blendvector!(short8, float4, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(short8, byte16, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(short8, short8, short8)(0, 0, 0);
    blendvector!(short8, short8, float4)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    blendvector!(short8, short8, byte16)(0, 0, 0); // { dg-error "mismatch in argument 3" }

    blendvector!(byte16, int4, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(byte16, short8, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(byte16, float4, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(byte16, byte16, int4)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    blendvector!(byte16, byte16, short8)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    blendvector!(byte16, byte16, float4)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    blendvector!(byte16, byte16, byte16)(0, 0, 0);
}

// The following declarations of the simd intrinsics are without any guards
// to verify `d/intrinsics.cc` is doing checks to prevent invalid lowerings.
V loadUnaligned(V)(const V*);
V storeUnaligned(V)(V*, V);

V0 shuffle(V0, V1, M)(V0, V1, M);

// Use overloads to test different argument positions.
template E(V) { alias typeof(V.array[0]) E; }
enum isV(T) = is(T : __vector(V[N]), V, size_t N);

__vector(E!V1[M.length]) shufflevector(V1, V2, M...)(V1, V2, M) if (isV!V1 && !isV!V2);
__vector(E!V2[M.length]) shufflevector(V1, V2, M...)(V1, V2, M) if (isV!V2 && !isV!V1);
__vector(E!V1[M.length]) shufflevector(V1, V2, M...)(V1, V2, M) if (isV!V1 && isV!V2);

V convertvector(V, T)(T);
V0 blendvector(V0, V1, M)(V0, V1, M);
