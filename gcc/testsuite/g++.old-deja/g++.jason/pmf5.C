// PRMS Id: 4985
// Build don't link:
// Special g++ Options:

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
};				// WARNING - implicit &
