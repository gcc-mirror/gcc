// Build don't link:
// Special g++ Options: -fexceptions

// testcase to check obstack allocation for cleanups

typedef unsigned char byte;
typedef unsigned short word16;
typedef unsigned long word32;
typedef unsigned char boolean;
enum {FALSE, TRUE};
extern "C" {
extern void __eprintf (const char *, const char *, unsigned, const char *);
}
extern "C" {
typedef unsigned int size_t;
extern void *memccpy(void *, const void *, int, size_t);
extern void *memchr(const void *, int, size_t);
extern void *memset(void *, int, size_t);
}
template <class T> struct SecBlock
{
public:
    SecBlock(unsigned int size)
        : size(size), ptr((new  T [(  size )]) ) {}
    ~SecBlock()
        {(memset(( ptr ), 0, (  size )*sizeof(*( ptr ))), delete [] ( ptr )) ;}
    operator T *() const
        {return ptr;}
    T *operator +(unsigned int offset)
        {return ptr+offset;}
    T& operator[](int index)
        {((void) (( index<size ) ? 0 : (__eprintf ("%s:%u: failed assertion `%s'\n",      "misc.h" ,   31 ,  "index<size" ), 0) )) ; return ptr[index];}
    const T& operator[](int index) const
        {((void) (( index<size ) ? 0 : (__eprintf ("%s:%u: failed assertion `%s'\n",      "misc.h" ,   33 ,  "index<size" ), 0) )) ; return ptr[index];}
    const unsigned int size;
    T *const ptr;
};
typedef SecBlock<byte> SecByteBlock;
void xorbuf(byte *buf, const byte *mask, unsigned int count);
void byteReverse(word16 *out, const word16 *in, unsigned int byteCount);
void byteReverse(word32 *out, const word32 *in, unsigned int byteCount);
inline word16 Invert(const word16 value)
{
    return (value << 8) | (value >> 8);
}
inline word32 Invert(const word32 value)
{
    word32 work = ((value & 0xFF00FF00L) >> 8) | ((value & 0x00FF00FFL) << 8);
    return (work << 16) | (work >> 16);
}
template <class T> inline T min (const T t1, const T t2)
{
    return (t1 < t2 ? t1 : t2);
}
template <class T> inline T max (const T t1, const T t2)
{
    return (t1 > t2 ? t1 : t2);
}
template <class T> inline void swap (T &a, T &b)
{
    T temp = a;
    a = b;
    b = temp;
}
template <class T> inline T rotl(T x, unsigned int y)
{
    return ((x<<y) | (x>>(sizeof(T)*8-y)));
}
template <class T> inline T rotr(T x, unsigned int y)
{
    return ((x>>y) | (x<<(sizeof(T)*8-y)));
}
int BytePrecision(unsigned long);
int BitPrecision(unsigned long);
unsigned long Crop(unsigned long, int size);
enum CipherDir {ENCRYPTION, DECRYPTION};
class BlockTransformation
{
public:
    virtual ~BlockTransformation() {}
    virtual void ProcessBlock(byte *inoutBlock) =0;
    virtual void ProcessBlock(const byte *inBlock, byte *outBlock) =0;
    virtual unsigned int BlockSize() const =0;
};
class StreamCipher
{
public:
    virtual ~StreamCipher() {}
    virtual byte ProcessByte(byte input) =0;
    virtual void ProcessString(byte *outString, const byte *inString, unsigned int length);
    virtual void ProcessString(byte *inoutString, unsigned int length);
};
class RandomAccessStreamCipher : public StreamCipher
{
public:
    virtual ~RandomAccessStreamCipher() {}
    virtual void Seek(unsigned long position) =0;
};
class RandomNumberGenerator
{
public:
    virtual ~RandomNumberGenerator() {}
    virtual byte GetByte() =0;
    virtual int GetBit();
    virtual word32 GetLong(word32 min=0, word32 max=0xffffffffL);
    virtual word16 GetShort(word16 min=0, word16 max=0xffff)
        {return (word16)GetLong(min, max);}
    virtual void GetBlock(byte *output, unsigned int size);
};
template <class T> void Shuffle(RandomNumberGenerator &rng, T *array, unsigned int size)
{
    while (--size)
        swap(array[size], array[(unsigned int)rng.GetLong(0, size)]);
}
class HashModule
{
public:
    virtual ~HashModule() {}
    virtual void Update(const byte *input, unsigned int length) =0;
    virtual void Final(byte *digest) =0;
    virtual int DigestSize() const =0;
    virtual void CalculateDigest(byte *digest, const byte *input, int length)
        {Update(input, length); Final(digest);}
};
class BufferedTransformation
{
public:
    virtual ~BufferedTransformation() {}
    virtual unsigned long MaxRetrieveable() =0;
    virtual void TransferTo(BufferedTransformation &target);
    virtual boolean Attachable() {return FALSE;}
    virtual void Detach(BufferedTransformation *) {}
    virtual void Attach(BufferedTransformation *) {}
    virtual void Close() {InputFinished();}
    virtual void Put(byte inByte) =0;
    virtual void Put(const byte *inString, unsigned int length) =0;
    virtual void InputFinished() {}
    void PutShort(word16 value, boolean highFirst=TRUE);
    void PutLong(word32 value, boolean highFirst=TRUE);
    virtual int Get(byte &outByte) =0;
    virtual unsigned int Get(byte *outString, unsigned int getMax) =0;
    int GetShort(word16 &value, boolean highFirst=TRUE);
    int GetLong(word32 &value, boolean highFirst=TRUE);
    unsigned int Skip(unsigned int skipMax);
};
class PK_CryptoSystem
{
public:
    virtual ~PK_CryptoSystem() {};
    virtual unsigned int MaxPlainTextLength() const =0;
    virtual unsigned int CipherTextLength() const =0;
};
class PK_Encryptor : public PK_CryptoSystem
{
public:
    virtual void Encrypt(RandomNumberGenerator &rng, const byte *plainText, unsigned int plainTextLength, byte *cipherText) =0;
};
class PK_Decryptor : public PK_CryptoSystem
{
public:
    virtual unsigned int Decrypt(const byte *cipherText, byte *plainText) =0;
};
class PK_SignatureSystem
{
public:
    virtual ~PK_SignatureSystem() {};
    virtual unsigned int MaxMessageLength() const =0;
    virtual unsigned int SignatureLength() const =0;
};
class PK_Signer : public PK_SignatureSystem
{
public:
    virtual void Sign(RandomNumberGenerator &rng, const byte *message, unsigned int messageLen, byte *signature) =0;
};
class PK_Verifier : public PK_SignatureSystem
{
public:
    virtual boolean Verify(const byte *message, unsigned int messageLen, const byte *signature) =0;
};
class ByteQueueNode;
class ByteQueue : public BufferedTransformation
{
public:
    ByteQueue();
    ~ByteQueue();
    unsigned long CurrentSize() const;
    unsigned long MaxRetrieveable()
        {return CurrentSize();}
    void Put(byte inByte);
    void Put(const byte *inString, unsigned int length);
    int Get(byte &outByte);
    unsigned int Get(byte *outString, unsigned int getMax);
private:
    ByteQueueNode *head, *tail;
};
enum ASNTag {INTEGER=0x02, BIT_STRING=0x03, SEQUENCE=0x10};
enum ASNIdFlag {CONSTRUCTED = 0x20};
unsigned int DERLengthEncode(unsigned int length, byte *output);
unsigned int DERLengthEncode(unsigned int length, BufferedTransformation &);
class BERDecodeErr {};
boolean BERLengthDecode(BufferedTransformation &, unsigned int &);
class BERSequenceDecoder : public BufferedTransformation
{
public:
    BERSequenceDecoder(BufferedTransformation &inQueue);
    ~BERSequenceDecoder();
    void Put(byte inByte) {}
    void Put(const byte *, unsigned int) {}
    unsigned long MaxRetrieveable()
        {return inQueue.MaxRetrieveable();}
    int Get(byte &outByte)
        {return inQueue.Get(outByte);}
    unsigned int Get(byte *outString, unsigned int getMax)
        {return inQueue.Get(outString, getMax);}
private:
    BufferedTransformation &inQueue;
    boolean definiteLength;
    unsigned int length;
};
class DERSequenceEncoder : public ByteQueue
{
public:
    DERSequenceEncoder(BufferedTransformation &outQueue);
    ~DERSequenceEncoder();
private:
    BufferedTransformation &outQueue;
};
extern "C" {
}
extern "C" {
extern void *memmove(void *, const void *, size_t);
extern char *strcpy(char *, const char *);
extern char *strncpy(char *, const char *, size_t);
extern char *strcat(char *, const char *);
extern char *strncat(char *, const char *, size_t);
extern int strcmp(const char *, const char *);
extern int strcoll(const char *, const char *);
extern int strncmp(const char *, const char *, size_t);
extern size_t strxfrm(char *, const char *, size_t);
extern void * __hide_memchr (const void *, int, size_t);
extern char * __hide_strchr (const char *, int);
extern size_t strcspn(const char *, const char *);
extern char * __hide_strpbrk (const char *, const char *);
extern char * __hide_strrchr (const char *, int);
extern size_t strspn(const char *, const char *);
extern char * __hide_strstr (const char *, const char *);
extern char *strtok(char *, const char *);
extern void *memset(void *, int, size_t);
extern char *strerror(int);
extern void *memccpy(void *, const void *, int, size_t);
extern char *strdup(const char *);
extern char *strsignal(int);
extern int ffs(const int);
extern int strcasecmp(const char *, const char *);
extern int strncasecmp(const char *, const char *, size_t);
}
typedef int ptrdiff_t;
extern "C" const char *strchr (const char *, int);
inline char *
strchr (char *s, int c)
{
  return (char*) strchr ((const char *) s, c);
}
extern "C" const char *strpbrk (const char *, const char *);
inline char *
strpbrk (char *s1, const char *s2)
{
  return (char *) strpbrk ((const char *) s1, s2);
}
extern "C" const char *strrchr (const char *, int);
inline char *
strrchr (char *s, int c)
{
  return (char *) strrchr ((const char *) s, c);
}
extern "C" const char *strstr (const char *, const char *);
inline char *
strstr (char *s1, const char *s2)
{
  return (char *) strstr ((const char *) s1, s2);
}
extern "C" void *memchr (const void *, int, size_t);
inline void *
memchr (void *s, int c, size_t n)
{
  return (void *) memchr ((const void *) s, c, n);
}
typedef word16 unit;
typedef short signedunit;
typedef unit *unitptr;
extern short global_precision;
boolean mp_addc
        (register unitptr r1,const unit * r2,register boolean carry);
boolean mp_subb
        (register unitptr r1,const unit * r2,register boolean borrow);
boolean mp_rotate_left(register unitptr r1,register boolean carry);
void mp_shift_right_bits(register unitptr r1,register short bits);
short mp_compare(const unit * r1,const unit * r2);
boolean mp_inc(register unitptr r);
boolean mp_dec(register unitptr r);
void mp_neg(register unitptr r);
void mp_init(register unitptr r, word16 value);
short significance(const unit * r);
int mp_udiv(register unitptr remainder,register unitptr quotient,
        const unit * dividend,const unit * divisor);
int mp_recip(register unitptr quotient,const unit * divisor);
int mp_div(register unitptr remainder,register unitptr quotient,
        unit * dividend, unit * divisor);
word16 mp_shortdiv(register unitptr quotient,
        const unit * dividend,register word16 divisor);
int mp_mod(register unitptr remainder,
        const unit * dividend,const unit * divisor);
word16 mp_shortmod(register unitptr dividend,register word16 divisor);
int mp_mult(register unitptr prod,
        const unit * multiplicand,const unit * multiplier);
int countbits(const unit * r);
int stage_peasant_modulus(const unit * n);
int stage_merritt_modulus(const unit * n);
int stage_upton_modulus(const unit * n);
int stage_smith_modulus(const unit * n);
int peasant_modmult(register unitptr prod,
        const unit * multiplicand,const unit * multiplier);
int merritt_modmult(register unitptr prod,
        const unit * multiplicand,const unit * multiplier);
int upton_modmult(register unitptr prod,
        const unit * multiplicand,const unit * multiplier);
int smith_modmult(register unitptr prod,
        const unit * multiplicand,const unit * multiplier);
void peasant_burn();
void merritt_burn();
void upton_burn();
void smith_burn();
int mp_modexp(register unitptr expout,const unit * expin,
        const unit * exponent,const unit * modulus);
int mp_modexp_crt(unitptr expout, const unit * expin,
        const unit * p, const unit * q, const unit * ep, const unit * eq, const unit * u);
word16 fetch_word16(byte *buf);
byte *put_word16(word16 w, byte *buf);
word32 fetch_word32(byte *buf);
byte *put_word32(word32 w, byte *buf);
int string_length(const char *s);
int str2reg(unit * reg,const char* digitstr);
int reg2str(char * s,const unit * n,short radix);
void mp_display(char * s,unitptr r);
word16 checksum(register byte * buf, register word16 count);
void cbc_xor(register unitptr dst, register unitptr src, word16 bytecount);
void hiloswap(byte * r1,short numbytes);
short mpi2reg(register unitptr r, register byte * buf);
short reg2mpi(register byte * buf, register unitptr r);
enum RandomNumberType {ANY, ODD, PRIME, BLUMINT};
class MPIRegister : public SecBlock<unit>
{
public:
    MPIRegister() : SecBlock<unit>((2048 / 16 ) ) {}
};
class ostream;
class bignum
{
public:
    bignum()
        {}
    bignum(unsigned long value);
    bignum(const char *str)
        {str2reg(reg, str);}
    enum Signedness{UNSIGNED, SIGNED};
    bignum(const byte *encodedBignum, unsigned int byteCount, Signedness s=UNSIGNED)
        {Decode(encodedBignum, byteCount, s);}
    bignum(const byte *BEREncodedInteger)
        {BERDecode(BEREncodedInteger);}
    bignum(BufferedTransformation &bt)
        {BERDecode(bt);}
    bignum(RandomNumberGenerator &rng, unsigned int bitcount)
        {Randomize(rng, bitcount);}
    bignum(RandomNumberGenerator &rng, const bignum &min, const bignum &max, RandomNumberType rnType=ANY)
        {Randomize(rng, min, max, rnType);}
    bignum(const bignum& t)
        {memcpy(reg, t.reg, (2048 /8) );}
    unsigned int Encode(byte *output) const;
    unsigned int Encode(byte *output, unsigned int outputLen) const;
    void Decode(const byte *input, unsigned int inputLen, Signedness=UNSIGNED);
    unsigned int DEREncode(byte *output) const;
    unsigned int DEREncode(BufferedTransformation &bt) const;
    void BERDecode(const byte *input);
    void BERDecode(BufferedTransformation &bt);
    void Randomize(RandomNumberGenerator &rng, unsigned int bitcount);
    void Randomize(RandomNumberGenerator &rng, const bignum &min, const bignum &max);
    void Randomize(RandomNumberGenerator &rng, const bignum &min, const bignum &max, RandomNumberType rnType);
    unsigned int ByteCount() const
    {
        return ((countbits( reg )+7)>>3) ;
    }
    int BitCount() const
    {
        return countbits(reg);
    }
    bignum&  operator++()
    {
        mp_inc(reg);
        return *this;
    }
    bignum&  operator--()
    {
        mp_dec(reg);
        return *this;
    }
    int      operator!() const
    {
        return ( ((* ((   reg.ptr   )+( global_precision )-1) ) ==(  0 )) && (significance( reg.ptr )<=1) ) ;
    }
    bignum&  operator=(const bignum& t)
    {
        memcpy(reg, t.reg, (2048 /8) );
        return *this;
    }
    bignum&  operator+=(const bignum& t)
    {
        mp_addc( reg ,  t.reg ,(boolean)0) ;
        return *this;
    }
    bignum&  operator-=(const bignum& t)
    {
        mp_subb( reg ,  t.reg ,(boolean)0) ;
        return *this;
    }
    bignum&  operator*=(const bignum& t)
    {
        *this = (*this) * t;
        return *this;
    }
    bignum&  operator/=(const bignum& t)
    {
        *this = (*this) / t;
        return *this;
    }
    bignum&  operator%=(const bignum& t)
    {
        *this = (*this) % t;
        return *this;
    }
    bignum&  operator<<=(unsigned int);
    bignum&  operator>>=(unsigned int);
    unsigned MaxBitPrecision() const {return 2048 ;}
    int operator[](unsigned int n) const;
    friend bignum operator+(bignum a, const bignum &b)
    {
        return (a+=b);
    }
    friend bignum operator-(bignum a, const bignum &b)
    {
        return (a-=b);
    }
    friend bignum operator*(const bignum &a, const bignum &b);
    friend bignum operator/(const bignum &a, const bignum &b);
    friend bignum operator%(const bignum &a, const bignum &b);
    friend bignum operator/(const bignum &a, word16 b);
    friend word16 operator%(const bignum &a, word16 b);
    friend bignum operator>>(bignum a, unsigned int n)
        {return (a>>=n);}
    friend bignum operator<<(bignum a, unsigned int n)
        {return (a<<=n);}
    void Negate() {mp_neg(reg);}
    friend bignum operator-(bignum a)
    {
        a.Negate();
        return a;
    }
    friend int    operator==(const bignum &a, const bignum &b)
    {
        return (memcmp(a.reg.ptr, b.reg.ptr, (2048 /8) )==0);
    }
    friend int    operator!=(const bignum& a, const bignum& b)
    {
        return (memcmp(a.reg.ptr, b.reg.ptr, (2048 /8) )!=0);
    }
    friend int    operator>(const bignum& a, const bignum& b)
    {
        return (mp_compare(a.reg, b.reg)>0);
    }
    friend int    operator>=(const bignum& a, const bignum& b)
    {
        return (mp_compare(a.reg, b.reg)>=0);
    }
    friend int    operator<(const bignum& a, const bignum& b)
    {
        return (mp_compare(a.reg, b.reg)<0);
    }
    friend int    operator<=(const bignum& a, const bignum& b)
    {
        return (mp_compare(a.reg, b.reg)<=0);
    }
    friend bignum a_times_b_mod_c(const bignum &x, const bignum& y, const bignum& m);
    friend bignum a_exp_b_mod_c(const bignum &x, const bignum& e, const bignum& m);
    class DivideErr {};
    friend void Divide(bignum &r, bignum &q,
                       const bignum &a, const bignum &d,
                       Signedness s=SIGNED);
    friend boolean Negative(const bignum &a)
    {
        return ((signedunit) (* (   a.reg.ptr   ) )  < 0) ;
    }
    friend bignum Abs(bignum a)
    {
        (((signedunit) (* (    a.reg.ptr    ) )  < 0)  ? (mp_neg( a.reg.ptr ),TRUE) : FALSE) ;
        return a;
    }
    friend ostream& operator<<(ostream& out, const bignum &a);
    unit lsUnit()   {return (* ((  reg.ptr  )+( global_precision )-1) ) ;}
private:
    MPIRegister reg;
};
class RSAPublicKey : public PK_Encryptor, public PK_Decryptor,
                     public PK_Verifier
{
public:
    RSAPublicKey(const bignum &n, const bignum &e);
    RSAPublicKey(BufferedTransformation &bt);
    void DEREncode(BufferedTransformation &bt) const;
    void Encrypt(RandomNumberGenerator &rng, const byte *plainText, unsigned int plainTextLength, byte *cipherText);
    unsigned int Decrypt(const byte *cipherText, byte *plainText);
    boolean Verify(const byte *message, unsigned int messageLen, const byte *signature);
    unsigned int MaxPlainTextLength() const {return modulusLen-11;}
    unsigned int CipherTextLength() const {return modulusLen;}
    unsigned int MaxMessageLength() const {return modulusLen-11;}
    unsigned int SignatureLength() const {return modulusLen;}
    const bignum& Exponent() const {return e;}
    const bignum& Modulus() const {return n;}
protected:
    void RawEncrypt(const bignum &in, bignum &out) const;
private:
    friend class RSAPrivateKey;
    RSAPublicKey() {}
    bignum n;            
    bignum e;
    unsigned int modulusLen;
};
class RSAPrivateKey : public RSAPublicKey, public PK_Signer
{
public:
    RSAPrivateKey(const bignum &n, const bignum &e, const bignum &d,
                  const bignum &p, const bignum &q, const bignum &dp, const bignum &dq, const bignum &u);
    RSAPrivateKey(RandomNumberGenerator &rng, int keybits, bignum eStart=17);
    RSAPrivateKey(BufferedTransformation &bt);
    void DEREncode(BufferedTransformation &bt) const;
    void Encrypt(const byte *plainText, unsigned int plainTextLength, byte *cipherText);
    unsigned int Decrypt(const byte *cipherText, byte *plainText);
    void Encrypt(RandomNumberGenerator &, const byte *plainText, unsigned int plainTextLength, byte *cipherText)
        {Encrypt(plainText, plainTextLength, cipherText);}
    void Sign(const byte *message, unsigned int messageLen, byte *signature)
        {Encrypt(message, messageLen, signature);}
    void Sign(RandomNumberGenerator &, const byte *message, unsigned int messageLen, byte *signature)
        {Encrypt(message, messageLen, signature);}
    unsigned int MaxMessageLength() const {return modulusLen-11;}
    unsigned int SignatureLength() const {return modulusLen;}
protected:
    void RawEncrypt(const bignum &in, bignum &out) const;
private:
    bignum d;    
    bignum p;    
    bignum q;
    bignum dp;
    bignum dq;
    bignum u;
};
bignum Gcd(const bignum &a, const bignum &b);
bignum Inverse(const bignum &a, const bignum &m);
boolean IsSmallPrime(const bignum &p);
boolean SmallDivisorsTest(const bignum &p);
boolean FermatTest(const bignum &p, unsigned int rounds);
boolean RabinMillerTest(RandomNumberGenerator &rng, const bignum &w, unsigned int rounds);
inline boolean IsPrime(const bignum &p)
{
    return (IsSmallPrime(p) || (SmallDivisorsTest(p) && FermatTest(p, 2)));
}
boolean NextPrime(bignum &p, const bignum &max, boolean blumInt=FALSE);
bignum a_exp_b_mod_pq(const bignum &a, const bignum &ep, const bignum &eq,
                      const bignum &p, const bignum &q, const bignum &u);
class PrimeAndGenerator
{
public:
    PrimeAndGenerator(RandomNumberGenerator &rng, unsigned int pbits);
    PrimeAndGenerator(RandomNumberGenerator &rng, unsigned int pbits, unsigned qbits);
    const bignum& Prime() const {return p;}
    const bignum& SubPrime() const {return q;}
    const bignum& Generator() const {return g;}
private:
    bignum p, q, g;
};
extern "C" {
extern void __eprintf (const char *, const char *, unsigned, const char *);
}
RSAPublicKey::RSAPublicKey(const bignum &n, const bignum &e)
    : n(n), e(e), modulusLen(n.ByteCount())
{
}
RSAPublicKey::RSAPublicKey(BufferedTransformation &bt)
{
    BERSequenceDecoder seq(bt);
    n.BERDecode(seq);
    modulusLen = n.ByteCount();
    e.BERDecode(seq);
}
void RSAPublicKey::DEREncode(BufferedTransformation &bt) const
{
    DERSequenceEncoder seq(bt);
    n.DEREncode(seq);
    e.DEREncode(seq);
}
void RSAPublicKey::Encrypt(RandomNumberGenerator &rng, const byte *input, unsigned int inputLen, byte *output)
{unsigned int i;
    ((void) (( inputLen <= MaxPlainTextLength() ) ? 0 : (__eprintf ("%s:%u: failed assertion `%s'\n",     "rsa.cpp" ,   30 ,  "inputLen <= MaxPlainTextLength()" ), 0) )) ;
    SecByteBlock pkcsBlock(modulusLen);
    pkcsBlock[0] = 0;
    pkcsBlock[1] = 2;   
    for (i = 2; i < modulusLen - inputLen - 1; i++)
    {
        do
            pkcsBlock[i] = rng.GetByte();
        while (pkcsBlock[i] == 0);
    }
    pkcsBlock[i++] = 0;      
    memcpy(pkcsBlock+i, input, inputLen);
    bignum c;
    RawEncrypt(bignum(pkcsBlock, modulusLen), c);
    c.Encode(output, modulusLen);
}
unsigned int RSAPublicKey::Decrypt(const byte *input, byte *output)
{unsigned int i;
    bignum m;
    RawEncrypt(bignum(input, modulusLen), m);
    SecByteBlock pkcsBlock(modulusLen);
    m.Encode(pkcsBlock, modulusLen);
    if ((pkcsBlock[0] != 0) || (pkcsBlock[1] != 1))
        return 0;
    for (i = 2; i < modulusLen-1; i++)
        if (pkcsBlock[i] != 0xff)
            break;
    if (pkcsBlock[i++] != 0)
        return 0;
    unsigned int outputLen = modulusLen - i;
    if (outputLen > MaxPlainTextLength())
        return 0;
    memcpy (output, pkcsBlock+i, outputLen);
    return outputLen;
}
boolean RSAPublicKey::Verify(const byte *message, unsigned int messageLen, const byte *signature)
{
    ((void) (( messageLen <= MaxMessageLength() ) ? 0 : (__eprintf ("%s:%u: failed assertion `%s'\n",     "rsa.cpp" ,   83 ,  "messageLen <= MaxMessageLength()" ), 0) )) ;
    SecByteBlock m(MaxMessageLength());
    unsigned int mLen = RSAPublicKey::Decrypt(signature, m);
    return (mLen==messageLen && memcmp(message, m, mLen)==0);
}
void RSAPublicKey::RawEncrypt(const bignum &in, bignum &out) const
{
    out = a_exp_b_mod_c(in, e, n);
}
RSAPrivateKey::RSAPrivateKey(const bignum &nIn, const bignum &eIn, const bignum &dIn,
                             const bignum &pIn, const bignum &qIn,
                             const bignum &dpIn, const bignum &dqIn, const bignum &uIn)
    : RSAPublicKey(nIn, eIn)
{
    d=dIn;
    if (pIn < qIn)
    {
        p=pIn;
        q=qIn;
        dp=dpIn;
        dq=dqIn;
    }
    else
    {
        p=qIn;
        q=pIn;
        dp=dqIn;
        dq=dpIn;
    }
    u=uIn;
}
RSAPrivateKey::RSAPrivateKey(RandomNumberGenerator &rng, int keybits, bignum eStart)
{
    p = bignum(rng, keybits/2, PRIME);
    const bignum minQ = ((bignum(1) << (keybits-1)) / p) + 1;
    const bignum maxQ = (bignum(1) << keybits) / p;
    do
    {
        bignum temp(rng, minQ, maxQ, PRIME);
        if (p>temp && p.BitCount()-(p-temp).BitCount() < 7)
        {
            q=p;
            p=temp;
            break;
        }
        if (p<temp && temp.BitCount()-(temp-p).BitCount() < 7)
        {
            q=temp;
            break;
        }
    } while (1);
    bignum phi = (p-1)*(q-1);
    for (e = eStart; Gcd(e, phi)!=1; ++e, ++e);
    d = Inverse(e, phi/Gcd(p-1, q-1));
    dp = d % (p-1);
    dq = d % (q-1);
    u = Inverse(p, q);
    n = p * q;
    modulusLen = n.ByteCount();
}
RSAPrivateKey::RSAPrivateKey(BufferedTransformation &bt)
{
    BERSequenceDecoder seq(bt);
    bignum version(seq);
    if (!!version)   
        throw BERDecodeErr() ;
    n.BERDecode(seq);
    modulusLen = n.ByteCount();
    e.BERDecode(seq);
    d.BERDecode(seq);
    p.BERDecode(seq);
    q.BERDecode(seq);
    dp.BERDecode(seq);
    dq.BERDecode(seq);
    u.BERDecode(seq);
    if (p>q)     
    {
        swap(p, q);
        swap(dp, dq);
    }
}
void RSAPrivateKey::DEREncode(BufferedTransformation &bt) const
{
    DERSequenceEncoder seq(bt);
    byte version[] = {INTEGER, 1, 0};
    seq.Put(version, sizeof(version));
    n.DEREncode(seq);
    e.DEREncode(seq);
    d.DEREncode(seq);
    q.DEREncode(seq);     
    p.DEREncode(seq);
    dq.DEREncode(seq);
    dp.DEREncode(seq);
    u.DEREncode(seq);
}
