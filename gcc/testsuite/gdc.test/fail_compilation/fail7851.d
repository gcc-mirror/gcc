// 7851


template TypeTuple(TList...)
{
    alias TList TypeTuple;
}

struct Tuple(Specs...)
{
    TypeTuple!(int, long, float) mem;

    alias Identity!(mem[0]) _0;
    alias Identity!(mem[1]) _1;
    alias Identity!(mem[2]) _2;

    alias mem this;

    enum length = mem.length;
}

private template Identity(alias T)
{
    alias T Identity;
}


void main() {
  alias Tuple!(int, long, float) TL;
  foreach (i; TL)
  { }
}

