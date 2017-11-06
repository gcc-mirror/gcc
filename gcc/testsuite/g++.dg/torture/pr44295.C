/* { dg-do compile } */
/* { dg-additional-options "-Wno-return-type" } */

extern "C" {
  typedef __SIZE_TYPE__ size_t;
  typedef struct   {
    } __sigset_t;
  typedef union {
  } pthread_barrierattr_t;
  }
  typedef unsigned short XMLCh;
      typedef unsigned long XMLSize_t;
  namespace xercesc_2_5 {
  class DOMNodeList;
  class DOMNode {
  };
  class DOMDocumentRange {
  };
  class DOMDocument: public DOMDocumentRange,  public DOMNode {
  };
  union wait   {
      struct       {
       }
  __wait_stopped;
    }
 div_t;
  class MemoryManager;
  class XMemory {
  public :     void* operator new(size_t size, MemoryManager* memMgr);
      void operator delete(void* p);
      void operator delete(void* p, MemoryManager* memMgr);
  };
  class XMLExcepts {
  public :     enum Codes     {
         NoError = 0       , HshTbl_ZeroModulus = 48       , HshTbl_BadHashFromKey = 49     };
  };
  class XMLUni {
  };
  }
  namespace xercesc_2_5 {
  class XMLException : public XMemory {
  public:
    virtual ~XMLException();
      XMLException(const char* const srcFile, const unsigned int srcLine, MemoryManager* const memoryManager = 0);
      void loadExceptText     (         const XMLExcepts::Codes toLoad     );
  };
  class XMLDeleter {
  };
  class XMLPlatformUtils {
  public :     static MemoryManager* fgMemoryManager;
   static inline size_t alignPointerForNewBlockAllocation(size_t ptrSize);
  };
  inline size_t XMLPlatformUtils::alignPointerForNewBlockAllocation(size_t ptrSize) {
  }
  class HashBase : public XMemory {
  public:
 virtual bool equals(const void *const key1, const void *const key2) = 0;
      virtual ~HashBase() {
};
  };
  class IllegalArgumentException : public XMLException {
  public:
IllegalArgumentException(const char* const srcFile , const unsigned int srcLine , const XMLExcepts::Codes toThrow , MemoryManager* memoryManager = 0) : XMLException(srcFile, srcLine, memoryManager) {
 loadExceptText(toThrow);
 }
  };
  class RuntimeException : public XMLException {
  public:
RuntimeException(const char* const srcFile , const unsigned int srcLine , const XMLExcepts::Codes toThrow , MemoryManager* memoryManager = 0) : XMLException(srcFile, srcLine, memoryManager) {
 loadExceptText(toThrow);
 }
  };
  class MemoryManager {
  public:
    virtual ~MemoryManager()     {
     }
      virtual void* allocate(size_t size) = 0;
  };
  template <class TElem> class BaseRefVectorOf : public XMemory {
      BaseRefVectorOf     (           const unsigned int maxElems         , const bool adoptElems = true         , MemoryManager* const manager = XMLPlatformUtils::fgMemoryManager     );
      virtual ~BaseRefVectorOf();
      bool fAdoptedElems;
      TElem** fElemList;
  };
  template <class TElem> BaseRefVectorOf<TElem>::BaseRefVectorOf( const unsigned int maxElems                                        , const bool adoptElems                                        , MemoryManager* const manager) :     fAdoptedElems(adoptElems) {
      for (unsigned int index = 0;
  index < maxElems;
  index++)         fElemList[index] = 0;
  }
  template <class TElem> BaseRefVectorOf<TElem>::~BaseRefVectorOf() {
  }
  class XMLString {
  public:
     static bool equals     (           const XMLCh* const str1         , const XMLCh* const str2     );
      static void moveChars     (                 XMLCh* const targetStr         , const XMLCh* const srcStr         , const unsigned int count     );
  };
  inline void XMLString::moveChars( XMLCh* const targetStr                                 , const XMLCh* const srcStr                                 , const unsigned int count) {
  }
  inline bool XMLString::equals( const XMLCh* const str1                                , const XMLCh* const str2) {
      const XMLCh* psz1 = str1;
      const XMLCh* psz2 = str2;
      if (psz1 == 0 || psz2 == 0) {
             return true;
     }
  }
  }
  namespace xercesc_2_5 {
  class HashPtr : public HashBase {
   virtual bool equals(const void *const key1, const void *const key2);
  };
  template <class TVal> struct DOMDeepNodeListPoolTableBucketElem : public XMemory {
      void* fKey1;
      XMLCh* fKey2;
      XMLCh* fKey3;
  };
  template <class TVal> class DOMDeepNodeListPool {
  public:
     DOMDeepNodeListPool     (         const XMLSize_t modulus       , const bool adoptElems       , const XMLSize_t initSize = 128     );
      TVal* getByKey(const void* const key1, const XMLCh* const key2, const XMLCh* const key3);
      DOMDeepNodeListPoolTableBucketElem<TVal>* findBucketElem(const void* const key1, const XMLCh* const key2, const XMLCh* const key3, XMLSize_t& hashVal);
      bool fAdoptedElems;
      DOMDeepNodeListPoolTableBucketElem<TVal>** fBucketList;
      XMLSize_t fHashModulus;
      HashBase* fHash;
      TVal** fIdPtrs;
      XMLSize_t fIdPtrsCount;
      MemoryManager* fMemoryManager;
  };
  template <class TVal> DOMDeepNodeListPool<TVal>::DOMDeepNodeListPool( const XMLSize_t modulus                                               , const bool adoptElems                                               , const XMLSize_t initSize) :   fAdoptedElems(adoptElems)     , fBucketList(0)     , fHash(0)     , fIdPtrs(0) {
      fHash = new (fMemoryManager) HashPtr();
      fIdPtrs = (TVal**) fMemoryManager->allocate(fIdPtrsCount * sizeof(TVal*));
   if (modulus == 0)         throw IllegalArgumentException("./xercesc/dom/impl/bad.c", 38, XMLExcepts::HshTbl_ZeroModulus, fMemoryManager);
  }
  template <class TVal> TVal* DOMDeepNodeListPool<TVal>::getByKey(const void* const key1, const XMLCh* const key2, const XMLCh* const key3) {
      XMLSize_t hashVal;
      DOMDeepNodeListPoolTableBucketElem<TVal>* findIt = findBucketElem(key1, key2, key3, hashVal);
  }
  template <class TVal> DOMDeepNodeListPoolTableBucketElem<TVal>* DOMDeepNodeListPool<TVal>:: findBucketElem(const void* const key1, const XMLCh* const key2, const XMLCh* const key3, XMLSize_t& hashVal) {
      if (hashVal > fHashModulus)         throw RuntimeException("./xercesc/dom/impl/bad.c", 64, XMLExcepts::HshTbl_BadHashFromKey, fMemoryManager);
      DOMDeepNodeListPoolTableBucketElem<TVal>* curElem = fBucketList[hashVal];
          if (fHash->equals(key1, curElem->fKey1) && (XMLString::equals(key2, curElem->fKey2)) && (XMLString::equals(key3, curElem->fKey3))) {
             return curElem;
     }
  }
  class DOMDeepNodeListImpl;
  class DOMDocumentImpl: public DOMDocument {
      DOMNodeList* getElementsByTagName(const XMLCh * tagname) const;
      DOMNodeList* getDeepNodeList(const DOMNode *rootNode, const XMLCh *tagName);
      DOMNodeList* getDeepNodeList(const DOMNode *rootNode,                                                  const XMLCh *namespaceURI,                                                  const XMLCh *localName);
      DOMDeepNodeListPool<DOMDeepNodeListImpl>* fNodeListPool;
  };
  }
  void * operator new(size_t amt, xercesc_2_5:: DOMDocument *doc);
  namespace xercesc_2_5 {
  class DOMNodeList {
  };
  class DOMDeepNodeListImpl: public DOMNodeList {
  };
  DOMNodeList *DOMDocumentImpl::getElementsByTagName(const XMLCh *tagname) const {
      return ((DOMDocumentImpl*)this)->getDeepNodeList(this,tagname);
  }
  DOMNodeList *DOMDocumentImpl::getDeepNodeList(const DOMNode *rootNode, const XMLCh *tagName) {
      if(!fNodeListPool) {
         fNodeListPool = new (this) DOMDeepNodeListPool<DOMDeepNodeListImpl>(109, false);
     }
      DOMNodeList* retList = fNodeListPool->getByKey(rootNode, tagName, 0);
  }
  DOMNodeList *DOMDocumentImpl::getDeepNodeList(const DOMNode *rootNode,                                                    const XMLCh *namespaceURI,                                                    const XMLCh *localName) {
      DOMNodeList* retList = fNodeListPool->getByKey(rootNode, localName, namespaceURI);
  }
  }

