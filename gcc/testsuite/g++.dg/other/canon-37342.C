class Asm;
template<typename _CharT> class basic_ostream;
typedef basic_ostream<char> ostream;
class Options {
    typedef void (Asm::* emitfunc_t) (ostream &);
    emitfunc_t getemit () const { return emitfunc; }
    emitfunc_t emitfunc;
};
