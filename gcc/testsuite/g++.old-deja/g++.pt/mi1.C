// { dg-do run  }
// Test that binfos aren't erroneously shared between instantiations.

class PK_CryptoSystem
{
};
class PK_Encryptor : public virtual PK_CryptoSystem
{
};
class PK_FixedLengthCryptoSystem : public virtual PK_CryptoSystem
{
public:
	virtual unsigned int CipherTextLength() const =0;
};
class PK_FixedLengthEncryptor : public virtual PK_Encryptor, public virtual PK_FixedLengthCryptoSystem
{
};
class PK_SignatureSystem
{
public:
	virtual ~PK_SignatureSystem() {};
};
class PK_Signer : public virtual PK_SignatureSystem
{
public:
	virtual void Sign() = 0;
};
class PK_Verifier : public virtual PK_SignatureSystem
{
};
class PK_Precomputation
{
};
template <class T> class
PK_WithPrecomputation : public T, public virtual PK_Precomputation
{
};
typedef PK_WithPrecomputation<PK_FixedLengthEncryptor> PKWPFLE;
typedef PK_WithPrecomputation<PK_Signer> PKWPS;
template <class EC> class
ECPublicKey : public PKWPFLE
{
public:
	unsigned int CipherTextLength() const { return 1; }
	EC ec;
};
template <class EC>
class ECPrivateKey : public ECPublicKey<EC>, public PKWPS
{
	void Sign() {}
	int d;
};
template <class EC>
class ECKEP : public ECPrivateKey<EC>
{
};
class GF2NT : public PK_CryptoSystem
{
	int t1;
};
class EC2N : public PK_CryptoSystem
{
	GF2NT field;
	int a;
};
template class ECKEP<EC2N>;
template class ECKEP<int>;

int
main ()
{
  ECKEP<EC2N> foo;

  return 0;
}

