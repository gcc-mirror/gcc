class cObject {
public:
    cObject *firstchildp;
};
class cHead : public cObject {
public:
    cObject *find(const char *objname) const;
};

