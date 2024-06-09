// PR c++/111660
// { dg-do compile { target c++11 } }

enum Value {
  LPAREN,
  RPAREN,
  LBRACE,
  RBRACE,
  LBRACK,
  RBRACK,
  CONDITIONAL,
  COLON,
  SEMICOLON,
  COMMA,
  PERIOD,
  BIT_OR,
  BIT_AND,
  BIT_XOR,
  BIT_NOT,
  NOT,
  LT,
  GT,
  MOD,
  ASSIGN,
  ADD,
  SUB,
  MUL,
  DIV,
  PRIVATE_NAME,
  STRING,
  TEMPLATE_SPAN,
  IDENTIFIER,
  WHITESPACE,
  ILLEGAL,
};

constexpr Value GetOneCharToken(char c) {
  return
      c == '(' ? LPAREN :
      c == ')' ? RPAREN :
      c == '{' ? LBRACE :
      c == '}' ? RBRACE :
      c == '[' ? LBRACK :
      c == ']' ? RBRACK :
      c == '?' ? CONDITIONAL :
      c == ':' ? COLON :
      c == ';' ? SEMICOLON :
      c == ',' ? COMMA :
      c == '.' ? PERIOD :
      c == '|' ? BIT_OR :
      c == '&' ? BIT_AND :
      c == '^' ? BIT_XOR :
      c == '~' ? BIT_NOT :
      c == '!' ? NOT :
      c == '<' ? LT :
      c == '>' ? GT :
      c == '%' ? MOD :
      c == '=' ? ASSIGN :
      c == '+' ? ADD :
      c == '-' ? SUB :
      c == '*' ? MUL :
      c == '/' ? DIV :
      c == '#' ? PRIVATE_NAME :
      c == '"' ? STRING :
      c == '\'' ? STRING :
      c == '`' ? TEMPLATE_SPAN :
      c == '\\' ? IDENTIFIER :
      c == ' ' ? WHITESPACE :
      c == '\t' ? WHITESPACE :
      c == '\v' ? WHITESPACE :
      c == '\f' ? WHITESPACE :
      c == '\r' ? WHITESPACE :
      c == '\n' ? WHITESPACE :
      ILLEGAL;
}

int main() {}
