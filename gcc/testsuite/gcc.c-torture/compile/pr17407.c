typedef struct libxml_xpathCallback { 
  void *ns_uri; 
} libxml_xpathCallback; 
 
typedef libxml_xpathCallback libxml_xpathCallbackArray[]; 
 
libxml_xpathCallbackArray *libxml_xpathCallbacks; 

void foo1(void);
 
void 
foo (void) 
{ 
  if ((*libxml_xpathCallbacks)[3].ns_uri != ((void *)0)) foo1(); 
} 
