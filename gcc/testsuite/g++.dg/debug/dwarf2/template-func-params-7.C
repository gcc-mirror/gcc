// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR debug/30161
// { dg-options "-gdwarf-2 -dA -fno-merge-debug-strings" }
// { dg-do compile { target c++11 } }

// There must be 5 subprograms generated:
// printf(const char*), printf<int, char, int>,
// printf<char, int>, printf<int> and foo().
// { dg-final {scan-assembler-times "DIE \\(0x\[^\n\]*\\) DW_TAG_subprogram" 5 } }

// That makes 6 template type parameters.
// { dg-final {scan-assembler-times "DIE \\(0x\[^\n\]*\\) DW_TAG_template_type_param" 6 } }
// { dg-final {scan-assembler-times "\"printf<int, char, int>.0\"\[^\n\]*DW_AT_name" 1 } }
// { dg-final {scan-assembler-times "\"printf<char, int>.0\"\[^\n\]*DW_AT_name" 1 } }
// { dg-final {scan-assembler-times "\"printf<int>.0\"\[^\n\]*DW_AT_name" 1 } }
// { dg-final {scan-assembler-times "\"printf.0\"\[^\n\]*DW_AT_name" 1 } }

// printf<int, char, int> and printf<char, int> have a pack expansion as
// function parameters. There should then be 3
// DW_TAG_GNU_template_parameter_pack and 3 DW_TAG_GNU_formal_parameter_pack DIEs
// { dg-final {scan-assembler-times "DIE \\(0x\[^\n\]*\\) DW_TAG_GNU_template_parameter_pack" 3 } }
// { dg-final {scan-assembler-times "DIE \\(0x\[^\n\]*\\) DW_TAG_GNU_formal_parameter_pack" 3 } }
// These 3 function template instantiations has a total of 3 template
// parameters named T.
// { dg_final {scan-assembler-times "\.ascii \"T.0\"\[\t \]+\[^\n\]*DW_AT_name" 3 } }


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
