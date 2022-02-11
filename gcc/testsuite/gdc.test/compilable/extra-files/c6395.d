// https://issues.dlang.org/show_bug.cgi?id=6395

template map(alias fun) {
  auto map(Range)(Range r) {
    struct Result
    {
      @property auto ref front()
      {
        return fun("a");
      }
    }
    return Result();
  }
}

Range find(alias pred, Range)(Range haystack) {
  pred(haystack.front);
  return haystack;
}

