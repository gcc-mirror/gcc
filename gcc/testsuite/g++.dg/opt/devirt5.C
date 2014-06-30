// PR c++/61659
// { dg-options "-O3" }
// { dg-final { scan-assembler-not "_ZN6parserIiE9getOptionEv" } }

struct generic_parser_base {
  virtual void getOption();
  void getExtraOptionNames() { getOption(); }
};
template <class DataType> struct parser : public generic_parser_base {
  virtual void getOption() {}
};
struct PassNameParser : public parser<int> {
  PassNameParser();
};
struct list {
  PassNameParser Parser;
  virtual void getExtraOptionNames() { return Parser.getExtraOptionNames(); }
};
list PassList;
