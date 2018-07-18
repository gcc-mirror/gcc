// { dg-do compile }

#include <string>
int main()
{
  wchar_t strs[4][2]= {  L"A", L"B", L"C" , L"D"};
  std::wstring ss(strs[0]);
  return 0;
}
