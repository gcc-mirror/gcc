// Testcase from P0170R1
// { dg-options -std=c++1z }

constexpr int AddEleven(int n){
  return[n]{return n+11;}();
}
static_assert(AddEleven(5)==16,"");
