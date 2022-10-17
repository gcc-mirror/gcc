/* REQUIRED_ARGS: -O
 * No divide-by-zero constant folding errors
 * https://issues.dlang.org/show_bug.cgi?id=20906
 */

int test12()
{
    int x = 0;
    int a = x && 1 / x;
    int b = !x || 1 / x;
    int c = x ? 1 / x : 1;
    int d = !x ? 1 : 1 / x;
    return a | b | c;
}
