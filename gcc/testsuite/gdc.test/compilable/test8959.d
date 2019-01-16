/*
TEST_OUTPUT:
---
U1 = int
U2 = int
V1 = long, K1 = string
V2 = long, K2 = string
TL1 = (int, string)
TL2 = (int, string)
U3 = int
U4 = int
V3 = long, K3 = string
V4 = long, K4 = string
TL3 = (int, string)
TL4 = (int, string)
---
*/

static if (is(int* == U1*, U1)) { pragma(msg, "U1 = ", U1); }
static if (is(int* :  U2*, U2)) { pragma(msg, "U2 = ", U2); }
static assert(is(int* == U*, U));
static assert(is(int* :  U*, U));

alias AA = long[string];
static if (is(AA == V1[K1], V1, K1)) { pragma(msg, "V1 = ", V1, ", K1 = ", K1); }
static if (is(AA :  V2[K2], V2, K2)) { pragma(msg, "V2 = ", V2, ", K2 = ", K2); }
static assert(is(AA == V[K], V, K));
static assert(is(AA :  V[K], V, K));

class B(TL...) {}
class C(TL...) : B!TL {}
alias X = C!(int, string);

static if (is(X == C!TL1, TL1...)) { pragma(msg, "TL1 = ", TL1); }
static if (is(X :  B!TL2, TL2...)) { pragma(msg, "TL2 = ", TL2); }
static assert(is(X == C!TL, TL...));
static assert(is(X :  B!TL, TL...));

void test8959()
{
    static if (is(int* == U3*, U3)) { pragma(msg, "U3 = ", U3); }
    static if (is(int* :  U4*, U4)) { pragma(msg, "U4 = ", U4); }
    static assert(is(int* == U*, U));
    static assert(is(int* :  U*, U));

    static if (is(AA == V3[K3], V3, K3)) { pragma(msg, "V3 = ", V3, ", K3 = ", K3); }
    static if (is(AA :  V4[K4], V4, K4)) { pragma(msg, "V4 = ", V4, ", K4 = ", K4); }
    static assert(is(AA == V[K], V, K));
    static assert(is(AA :  V[K], V, K));

    static if (is(X == C!TL3, TL3...)) { pragma(msg, "TL3 = ", TL3); }
    static if (is(X :  B!TL4, TL4...)) { pragma(msg, "TL4 = ", TL4); }
    static assert(is(X == C!TL, TL...));
    static assert(is(X :  B!TL, TL...));
}
