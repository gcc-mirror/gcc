// Build don't link: 
// GROUPS passed static-mem

class desc;

class a_desc {
    public:

    static desc the_desc;

    virtual desc *get_desc();
};

class desc : public a_desc {
    public:

    static desc the_desc;

    desc(int);

    desc *get_desc();
};


desc desc::the_desc(1);

desc a_desc::the_desc(0);
