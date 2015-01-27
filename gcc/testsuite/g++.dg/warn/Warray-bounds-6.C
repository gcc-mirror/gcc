// { dg-do compile }
// { dg-options "-O3 -Warray-bounds" }

struct type {
    bool a, b;
    bool get_b() { return b; }
};

type stuff[9u];

void bar();

void foo()
{
  for(unsigned i = 0u; i < 9u; i++)
    {
      if(!stuff[i].a)
	continue;

      bar();

      for(unsigned j = i + 1u; j < 9u; j++)
	if(stuff[j].a && stuff[j].get_b()) // { dg-bogus "above array bounds" }
	  return;
    }
}
