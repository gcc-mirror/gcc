/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-fnsplit" } */
class MemoryManager;
class XMLExcepts {
public : 
    enum Codes     {
      AttrList_BadIndex
    };
};
class XMLException {
public:
    XMLException(const char* const srcFile, const unsigned int srcLine,
MemoryManager* const memoryManager = 0);
};
class ArrayIndexOutOfBoundsException : public XMLException {
public:
    ArrayIndexOutOfBoundsException(const char* const srcFile , const unsigned
int srcLine , const XMLExcepts::Codes toThrow , MemoryManager* memoryManager =
0) : XMLException(srcFile, srcLine, memoryManager) {
    }
};
class XMLAttDef {
  bool fExternalAttribute;
};
class XMLAttDefList {
public:
    MemoryManager* getMemoryManager() const;
};
class DTDAttDef : public XMLAttDef {
};
class DTDAttDefList : public XMLAttDefList {
  virtual const XMLAttDef &getAttDef(unsigned int index) const ;
  DTDAttDef** fArray;
  unsigned int fCount;
};
const XMLAttDef &DTDAttDefList::getAttDef(unsigned int index) const {
  if(index >= fCount) 
    throw ArrayIndexOutOfBoundsException("foo.cpp", 0,
XMLExcepts::AttrList_BadIndex, getMemoryManager());
  return *(fArray[index]);
}

/* Mistake in branch prediction caused us to split away real body of the function keeping
   only throw () invokation.   This is bad idea.  */
/* { dg-final { scan-tree-dump-not "Splitting function" "fnsplit"} } */
/* { dg-final { cleanup-tree-dump "fnsplit" } } */
