fn main () {
// { dg-do compile }
// { dg-options "-gdwarf-5 -dA -w" }
    let x = (32, 32);
// Look for field __0 and __1
// { dg-final { scan-assembler "__0" } } */
// { dg-final { scan-assembler "__1" } } */
}
