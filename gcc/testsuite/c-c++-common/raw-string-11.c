// PR preprocessor/48740
// { dg-do run { target { c || c++11 } } }
// { dg-options "-std=gnu99 -trigraphs -save-temps" { target c } }
// { dg-options "-save-temps" { target c++ } }

int main ()
{
  return __builtin_memcmp (R"raw(foo%sbar%sfred%sbob?????)raw",
			   "foo%sbar%sfred%sbob?""?""?""?""?",
			   sizeof ("foo%sbar%sfred%sbob?""?""?""?""?"));
}
