// { dg-do assemble  }
// prms-id: 9866
class TChar 
	{ 
public: 
	explicit inline TChar(unsigned int aChar); 
	inline operator unsigned int() const; 
private: 
	unsigned int iChar; 
   }; 
inline TChar::TChar(unsigned int aChar) 
	: iChar(aChar) 
	{} 
inline TChar::operator unsigned int() const 
	{return(iChar);} 
 
class TDes8 
   { 
public: 
   inline const unsigned char &operator[](int anIndex) const; 
	const unsigned char &AtC(int anIndex) const; 
   }; 
 
inline const unsigned char &TDes8::operator[](int anIndex) const 
	{return(AtC(anIndex));} 
 
 
void doExponent(TDes8 &aDigBuf) 
            { 
            2 ? TChar(aDigBuf[2]) : '0';
            } 
