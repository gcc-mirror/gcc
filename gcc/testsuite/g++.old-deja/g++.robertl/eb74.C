// { dg-do assemble  }
class base {
public:
        virtual ~base();
};


class foo : public base {

public :

     foo (char *cs);

     virtual void op (unsigned char dummy = false);
     unsigned char m_dummy;
};


void foo :: op ( unsigned char dummy)

{
    bool bar;

    if (dummy) {
        foo IT_tempPhase( 0 );
        return;
    }

    if ((m_dummy || bar)) {

    }

}
