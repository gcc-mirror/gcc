// Build don't link: 
// GROUPS passed conversions
// cvt file
// Message-Id: <9307200528.AA02094@legolas>
// From: Mark Kuzmycz <kuzmycz@deakin.edu.au>
// Subject: int & conversion operator
// Date: Tue, 20 Jul 93 15:28:47 EST

class Int
{
	public:
		Int(void);
		Int(int);
		Int(const Int&);
	
		Int* copy(void) const;
		
		operator int&();
		
		Int& operator ++(void);
		Int& operator --(void);
		
	private:
		int value;
};
 
int main()
{
  Int data = 2;
  Int test;

  test = data * 12;
  data += 1;
}

// UNKNOWN "FAIL"
