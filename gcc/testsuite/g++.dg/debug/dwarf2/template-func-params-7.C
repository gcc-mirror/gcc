// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR debug/30161
// { dg-options "-g -dA -std=c++0x" }
// { dg-do compile }

// There must be 5 subprograms generated:
// printf(const char*), printf<int, char, int>,
// printf<char, int>, printf<int> and foo().
// { dg-final {scan-assembler-times "DIE \\(0x.*?\\) DW_TAG_subprogram" 5 } }

// That makes 6 template type parameters.
// { dg-final {scan-assembler-times "DIE \\(0x.*?\\) DW_TAG_template_type_param" 6 } }
// { dg-final {scan-assembler-times "DW_AT_name: \"printf<int, char, int>\"" 1 } }
// { dg-final {scan-assembler-times "DW_AT_name: \"printf<char, int>\"" 1 } }
// { dg-final {scan-assembler-times "DW_AT_name: \"printf<int>\"" 1 } }
// { dg-final {scan-assembler-times "DW_AT_name: \"printf\"" 1 } }

// printf<int, char, int> and printf<char, int> have a pack expansion as
// function parameters. In the former, the elements of the parameter pack
// expansion are PackTypes#0, PackTypes#1 and the arguments are args#0 and
// args#1. In the later, the element of the parameter pack expansion
// is PackTypes#0 and the argument is args#0.
// { dg-final {scan-assembler-times "DW_AT_name: \"PackTypes#0\"" 2 } }
// { dg-final {scan-assembler-times "DW_AT_name: \"args#0\"" 2 } }
// { dg-final {scan-assembler-times "DW_AT_name: \"PackTypes#1\"" 1 } }
// { dg-final {scan-assembler-times "DW_AT_name: \"args#1\"" 1 } }

// { dg_final {scan-assembler-times "\.ascii \"T.0\"\[\t \]+.*?DW_AT_name" 3 } }

void
printf(const char* s)
{
  /* Commented this to not pull std::cout into what should be
     a simple test.
  while (*s)
    std::cout << *s++;
  */
}

template<typename T, typename... PackTypes>
void
printf(const char* s,
       T value,
       PackTypes... args)
{
  while (*s)
    {
      if (*s == '%' && *++s != '%')
	{
	  /* std::cout << value; */
	  return printf(++s, args...);
	}
    }
}

void
foo ()
{
  int x;
  printf("%c %d", x, 'x', 3);
}
