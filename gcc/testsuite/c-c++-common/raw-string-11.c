// PR preprocessor/48740
// { dg-options "-std=gnu99 -trigraphs -save-temps" { target c } }
// { dg-options "-std=c++0x -save-temps" { target c++ } }
// { dg-do run }

int main ()
{
  return __builtin_memcmp (R"raw(foo%sbar%sfred%sbob?????)raw",
			   "foo%sbar%sfred%sbob?""?""?""?""?",
			   sizeof ("foo%sbar%sfred%sbob?""?""?""?""?"));
}

// { dg-final { cleanup-saved-temps } }
