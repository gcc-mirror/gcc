// { dg-do assemble  }
// { dg-options "" }
// PRMS Id: 4985

struct Thing {
        int OverloadFn() const;
        void FunctionA(char* restOfLine);
        void OverloadFn(char* restOfLine);
};

struct ThingEntry {
        void (Thing::*_handler)(char* restOfLine);
};

static ThingEntry KeyWordTable[] = {
        &Thing::FunctionA,
        Thing::OverloadFn,
};				// { dg-error "assuming" "assuming" } implicit &
// { dg-message "note" "note" { target *-*-* } .-1 }
