// Build don't link: 

class baseClassA
{
public:
private:
	class internalClassA
	{
	public:
	private:
		typedef int privateType;

		privateType memberA;
	};
};

class baseClassB
{
public:
private:
	class internalClassA
	{
	public:
	private:
		typedef unsigned int privateType;

		privateType memberB;
	};
};
