// Build don't link: 
// GROUPS passed default-functions
class dictionary {
public:
  dictionary (int);
};

class symbol {
public:
  symbol ();
};

// a default ctor should not be generated for hyphenation_language,
// since a ctor has already been declared; if one is generated, there
// will be an error about not enough args to the ctor for dictionary,
// since dictionary only defines a ctor taking an int (it ALSO should
// not get a default ctor)
struct hyphenation_language {
  symbol name;
  dictionary exceptions;
  hyphenation_language(symbol nm) : name(nm), exceptions(501) {}
};
