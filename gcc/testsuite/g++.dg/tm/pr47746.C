// { dg-do compile }
// { dg-options "-fgnu-tm" }

class InputStream
{
	public:
	virtual unsigned int readUint32 () = 0;
};

class Building
{
	public:
	__attribute__((transaction_safe)) Building (InputStream *stream);
	__attribute__((transaction_safe)) void freeGradients ();
	void load (InputStream *stream);
};

Building::Building (InputStream *stream)
{
	load(stream);
}

void Building::load (InputStream *stream)
{
	int j = (int)stream->readUint32 ();
	freeGradients ();
}
