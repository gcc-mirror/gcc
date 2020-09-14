// { dg-do assemble  }
// GROUPS passed initialization
// init file
// From: Ingo Donasch <ingo@specs.de>
// Date:     Wed, 16 Jun 93 13:28:55 +01:00
// Subject:  g++-2.4.2 bug report
// Message-ID: <199306161128.AA22079@opamp.specs.de>

class b;

class a {
public:
        a(const b*);            // needs this parameter
};

class b {
private:
        a three[3];
public:
        b();
};

b::b() : three(this)  // { dg-error "array|could not convert" }
{
}

