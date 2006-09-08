// PR c++/26957

struct LongDouble {
    char ld[16];
};

struct DynAny  {
    virtual void insert_longdouble(LongDouble value) = 0;
};

struct TAO_DynCommon : public virtual DynAny {
    virtual void insert_longdouble (LongDouble value);
};

void TAO_DynCommon::insert_longdouble (LongDouble value) { }

