// PR c++/13543
// { dg-do compile }
// { dg-options "-O3" }

struct basic_string { basic_string(const basic_string&); };
basic_string operator+(const basic_string& lhs, char);
void dumpNode(basic_string start) { dumpNode(start + 'a'); }
