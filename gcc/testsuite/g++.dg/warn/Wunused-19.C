// PR c++/104702
// { dg-additional-options "-fno-exceptions -Wunused-value" }

struct FlyString {
  FlyString(char const*);
  ~FlyString();
};

struct Array { FlyString __data[1]; };

void frobnicate(Array&);

int main() {
    Array s_reserved_words = { "" }; // { dg-bogus "value computed is not used" }
    frobnicate(s_reserved_words);
}
