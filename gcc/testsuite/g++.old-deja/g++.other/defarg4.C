// Build don't link:
// Origin: scott snyder <snyder@fnal.gov>

class complex
{
public:
  complex();
};

struct MLC33
{
  MLC33( const complex& = complex() );
};

void EmptyClone()
{
  MLC33();
}

void makeM33()
{
  MLC33();
}

void Clone()
{
  MLC33();
}
