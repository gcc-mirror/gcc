// https://issues.dlang.org/show_bug.cgi?id=11717

enum int[4] A = [1,2,3,4];
enum int[4] B = [1,2,3,4];
enum int[4] C = A[] + B[];
static assert(C == [2, 4, 6, 8]);

enum int[2] D1 = A[1..3] * B[2..4];
static assert(D1 == [6, 12]);

enum int[2] D2 = A[1..3] * 6;
static assert(D2 == [12, 18]);

enum int[2] D3 = 5 - A[1..3];
static assert(D3 == [3, 2]);

enum int[2][2] D4 = [D1, D2] + [D2, D3];
static assert(D4 == [[18, 30], [15, 20]]);

enum int[2][2] D5 = [[18, 30], [15, 20]] + [12, 18];
static assert(D5 == [[30, 48], [27, 38]]);

import core.simd;

static if (__traits(compiles, int4))
{
    enum int4 D = [1,2,3,4];
    enum int4 E = [1,2,3,4];
    enum int4 F = D * E;
    static assert(F.array == [1, 4, 9, 16]);
}
