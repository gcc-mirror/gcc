/* { dg-do compile } */
/* { dg-options "-O2" } */
/* PRE testcase for PR 24231, problem with PRE coalescing abnormal phis.  */
struct MemoryManager {
      virtual void deallocate() = 0;
};
struct XalanVector {
      ~XalanVector()   {
	      m_memoryManager->deallocate();
	        }
        void swap(XalanVector& theOther)   {
	        MemoryManager* const theTempManager = m_memoryManager;
		    m_memoryManager = theOther.m_memoryManager;
		        theOther.m_memoryManager = theTempManager;
			    theOther.m_size = 0;
			      }
	  void push_back()   {
	          XalanVector theTemp(*this);
		      theTemp.push_back();
		          swap(theTemp);
			    }
	    MemoryManager* m_memoryManager;
	      int m_size;
};
void f(void) {
      XalanVector tempVector;
        tempVector.push_back();
}
