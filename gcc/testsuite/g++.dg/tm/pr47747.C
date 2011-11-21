// { dg-do compile }
// { dg-options "-fgnu-tm -O" }

class InputStream
{
        public:
//        __attribute__((transaction_safe))
        virtual unsigned int readUint32 () = 0;
};

class Building
{
        public:
        __attribute__((transaction_safe))
        Building (InputStream *stream);
};

Building::Building (InputStream *stream)
{
        stream->readUint32 (); /* { dg-error "InputStream::readUint32" } */
}
