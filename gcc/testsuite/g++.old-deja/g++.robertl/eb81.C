// { dg-do assemble  }
class   x
{
public:
        x   (int init_buffer_size=0);
        ~x   ();
};



class   xSequence
{
public:
        xSequence   ();
        ~xSequence   ();
        x   Get(int index)const;
};



class   foo
{
public:
        bool bar(const x   & name, x    & value);

};



bool foo::bar(const x  & name, x    & value)
{
        bool result = false;

        xSequence    seq;
        x    v1, v2;
        if(result ? bar(seq.Get(1),v2) : bar(seq.Get(2),v2))

                ;

        return result;
}

