// PERMUTE_ARGS:

extern(C) int printf(const char*, ...);

interface IWriter
{
        IWriter put (I1 x);
}

interface I1
{
        void write (IWriter writer);
}

interface I2 : I1 {}

interface I3 : I2 {}

class Newline : I3
{
        static int OKset;

        void write (IWriter writer)
        {
                printf ("OK\n");
                OKset += 1;
        }
}



class Writer : IWriter
{
        IWriter put (I1 x)
        {
                x.write (this);
                return this;
        }
}

class FlushWriter : Writer
{
        override IWriter put (I1 x)
        {
               // have superclass handle the I1
                super.put (x);

                // flush output when we see a newline
                if (cast(Newline) x)
                   {
                   }

                return this;
        }
}



void test (IWriter w)
{
        //w.put (new Newline);

        I3 NL = new Newline;
        w.put (NL);
}


int main()
{
        test (new FlushWriter);
        assert(Newline.OKset == 1);
        return 0;
}


