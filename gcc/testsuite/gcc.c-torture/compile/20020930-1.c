/* PR c/8002 */

float expm1f(float x) {
     union {
         float value;
         unsigned word;
     } sf_u;
     sf_u.word = (unsigned) x * 2;
     return x + sf_u.value;
}
