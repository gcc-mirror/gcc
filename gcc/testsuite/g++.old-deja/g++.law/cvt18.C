// { dg-do assemble  }
// GROUPS passed conversions
// cvt file
// Message-Id: <9307090042.AA28565@uluru.Stanford.EDU>
// From: interran@uluru.stanford.edu (John Interrante)
// Subject: gcc 2.4.5 on sparc-sun-sunos4.1.2: automatic conversion functions
// Date: Thu, 8 Jul 93 17:42:12 PDT

class Token {
public:
    operator const void*() const;
    int operator !=(int code) const;
private:
    int code_;
};

extern Token next_token();

void foo(Token token) {
    if (token && token != '{') {}

    const int inside_body = 1;
    if (inside_body && (token = next_token())) {}
}
