// -*- C++ -*-

#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <ctype.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <sys/types.h>
#include <time.h>
#include <signal.h>
#include <pthread.h>

void xmlCheckVersion (int version);
typedef unsigned char xmlChar;
xmlChar *xmlStrdup (const xmlChar * cur);
xmlChar *xmlStrndup (const xmlChar * cur, int len);
xmlChar *xmlCharStrndup (const char *cur, int len);
xmlChar *xmlCharStrdup (const char *cur);
xmlChar *xmlStrsub (const xmlChar * str, int start, int len);
const xmlChar *xmlStrchr (const xmlChar * str, xmlChar val);
const xmlChar *xmlStrstr (const xmlChar * str, const xmlChar * val);
const xmlChar *xmlStrcasestr (const xmlChar * str, const xmlChar * val);
int xmlStrcmp (const xmlChar * str1, const xmlChar * str2);
int xmlStrncmp (const xmlChar * str1, const xmlChar * str2, int len);
int xmlStrcasecmp (const xmlChar * str1, const xmlChar * str2);
int xmlStrncasecmp (const xmlChar * str1, const xmlChar * str2, int len);
int xmlStrEqual (const xmlChar * str1, const xmlChar * str2);
int xmlStrQEqual (const xmlChar * pref,
		  const xmlChar * name, const xmlChar * str);
int xmlStrlen (const xmlChar * str);
xmlChar *xmlStrcat (xmlChar * cur, const xmlChar * add);
xmlChar *xmlStrncat (xmlChar * cur, const xmlChar * add, int len);
xmlChar *xmlStrncatNew (const xmlChar * str1, const xmlChar * str2, int len);
int xmlStrPrintf (xmlChar * buf, int len, const xmlChar * msg, ...);
int xmlStrVPrintf (xmlChar * buf, int len, const xmlChar * msg, va_list ap);
int xmlGetUTF8Char (const unsigned char *utf, int *len);
int xmlCheckUTF8 (const unsigned char *utf);
int xmlUTF8Strsize (const xmlChar * utf, int len);
xmlChar *xmlUTF8Strndup (const xmlChar * utf, int len);
const xmlChar *xmlUTF8Strpos (const xmlChar * utf, int pos);
int xmlUTF8Strloc (const xmlChar * utf, const xmlChar * utfchar);
xmlChar *xmlUTF8Strsub (const xmlChar * utf, int start, int len);
int xmlUTF8Strlen (const xmlChar * utf);
int xmlUTF8Size (const xmlChar * utf);
int xmlUTF8Charcmp (const xmlChar * utf1, const xmlChar * utf2);

typedef struct _xmlParserInputBuffer xmlParserInputBuffer;
typedef xmlParserInputBuffer *xmlParserInputBufferPtr;
typedef struct _xmlOutputBuffer xmlOutputBuffer;
typedef xmlOutputBuffer *xmlOutputBufferPtr;
typedef struct _xmlParserInput xmlParserInput;
typedef xmlParserInput *xmlParserInputPtr;
typedef struct _xmlParserCtxt xmlParserCtxt;
typedef xmlParserCtxt *xmlParserCtxtPtr;
typedef struct _xmlSAXLocator xmlSAXLocator;
typedef xmlSAXLocator *xmlSAXLocatorPtr;
typedef struct _xmlSAXHandler xmlSAXHandler;
typedef xmlSAXHandler *xmlSAXHandlerPtr;
typedef struct _xmlEntity xmlEntity;
typedef xmlEntity *xmlEntityPtr;

typedef enum
{
  XML_BUFFER_ALLOC_DOUBLEIT,
  XML_BUFFER_ALLOC_EXACT,
  XML_BUFFER_ALLOC_IMMUTABLE,
  XML_BUFFER_ALLOC_IO,
  XML_BUFFER_ALLOC_HYBRID
} xmlBufferAllocationScheme;

typedef struct _xmlBuffer xmlBuffer;
typedef xmlBuffer *xmlBufferPtr;

struct _xmlBuffer
{
  xmlChar *content;
  unsigned int use;
  unsigned int size;
  xmlBufferAllocationScheme alloc;
  xmlChar *contentIO;
};

typedef struct _xmlBuf xmlBuf;
typedef xmlBuf *xmlBufPtr;

xmlChar *xmlBufContent (const xmlBuf * buf);
xmlChar *xmlBufEnd (xmlBufPtr buf);
size_t xmlBufUse (const xmlBufPtr buf);
size_t xmlBufShrink (xmlBufPtr buf, size_t len);

typedef enum
{
  XML_ELEMENT_NODE = 1,
  XML_ATTRIBUTE_NODE = 2,
  XML_TEXT_NODE = 3,
  XML_CDATA_SECTION_NODE = 4,
  XML_ENTITY_REF_NODE = 5,
  XML_ENTITY_NODE = 6,
  XML_PI_NODE = 7,
  XML_COMMENT_NODE = 8,
  XML_DOCUMENT_NODE = 9,
  XML_DOCUMENT_TYPE_NODE = 10,
  XML_DOCUMENT_FRAG_NODE = 11,
  XML_NOTATION_NODE = 12,
  XML_HTML_DOCUMENT_NODE = 13,
  XML_DTD_NODE = 14,
  XML_ELEMENT_DECL = 15,
  XML_ATTRIBUTE_DECL = 16,
  XML_ENTITY_DECL = 17,
  XML_NAMESPACE_DECL = 18,
  XML_XINCLUDE_START = 19,
  XML_XINCLUDE_END = 20, XML_DOCB_DOCUMENT_NODE = 21
} xmlElementType;

typedef struct _xmlNotation xmlNotation;
typedef xmlNotation *xmlNotationPtr;

struct _xmlNotation
{
  const xmlChar *name;
  const xmlChar *PublicID;
  const xmlChar *SystemID;
};

typedef enum
{
  XML_ATTRIBUTE_CDATA = 1,
  XML_ATTRIBUTE_ID,
  XML_ATTRIBUTE_IDREF,
  XML_ATTRIBUTE_IDREFS,
  XML_ATTRIBUTE_ENTITY,
  XML_ATTRIBUTE_ENTITIES,
  XML_ATTRIBUTE_NMTOKEN,
  XML_ATTRIBUTE_NMTOKENS,
  XML_ATTRIBUTE_ENUMERATION,
  XML_ATTRIBUTE_NOTATION
} xmlAttributeType;

typedef enum
{
  XML_ATTRIBUTE_NONE = 1,
  XML_ATTRIBUTE_REQUIRED,
  XML_ATTRIBUTE_IMPLIED,
  XML_ATTRIBUTE_FIXED
} xmlAttributeDefault;

typedef struct _xmlEnumeration xmlEnumeration;
typedef xmlEnumeration *xmlEnumerationPtr;

struct _xmlEnumeration
{
  struct _xmlEnumeration *next;
  const xmlChar *name;
};

typedef struct _xmlAttribute xmlAttribute;
typedef xmlAttribute *xmlAttributePtr;
struct _xmlAttribute
{
  void *_private;
  xmlElementType type;
  const xmlChar *name;
  struct _xmlNode *children;
  struct _xmlNode *last;
  struct _xmlDtd *parent;
  struct _xmlNode *next;
  struct _xmlNode *prev;
  struct _xmlDoc *doc;

  struct _xmlAttribute *nexth;
  xmlAttributeType atype;
  xmlAttributeDefault def;
  const xmlChar *defaultValue;
  xmlEnumerationPtr tree;
  const xmlChar *prefix;
  const xmlChar *elem;
};

typedef enum
{
  XML_ELEMENT_CONTENT_PCDATA = 1,
  XML_ELEMENT_CONTENT_ELEMENT,
  XML_ELEMENT_CONTENT_SEQ,
  XML_ELEMENT_CONTENT_OR
} xmlElementContentType;

typedef enum
{
  XML_ELEMENT_CONTENT_ONCE = 1,
  XML_ELEMENT_CONTENT_OPT,
  XML_ELEMENT_CONTENT_MULT,
  XML_ELEMENT_CONTENT_PLUS
} xmlElementContentOccur;

typedef struct _xmlElementContent xmlElementContent;
typedef xmlElementContent *xmlElementContentPtr;

struct _xmlElementContent
{
  xmlElementContentType type;
  xmlElementContentOccur ocur;
  const xmlChar *name;
  struct _xmlElementContent *c1;
  struct _xmlElementContent *c2;
  struct _xmlElementContent *parent;
  const xmlChar *prefix;
};

typedef enum
{
  XML_ELEMENT_TYPE_UNDEFINED = 0,
  XML_ELEMENT_TYPE_EMPTY = 1,
  XML_ELEMENT_TYPE_ANY,
  XML_ELEMENT_TYPE_MIXED,
  XML_ELEMENT_TYPE_ELEMENT
} xmlElementTypeVal;

typedef struct _xmlRegexp xmlRegexp;
typedef xmlRegexp *xmlRegexpPtr;
typedef struct _xmlRegExecCtxt xmlRegExecCtxt;
typedef xmlRegExecCtxt *xmlRegExecCtxtPtr;
typedef struct _xmlDict xmlDict;
typedef xmlDict *xmlDictPtr;

int xmlInitializeDict (void);
xmlDictPtr xmlDictCreate (void);
size_t xmlDictSetLimit (xmlDictPtr dict, size_t limit);
size_t xmlDictGetUsage (xmlDictPtr dict);
xmlDictPtr xmlDictCreateSub (xmlDictPtr sub);
int xmlDictReference (xmlDictPtr dict);
void xmlDictFree (xmlDictPtr dict);
const xmlChar *xmlDictLookup (xmlDictPtr dict, const xmlChar * name, int len);
const xmlChar *xmlDictExists (xmlDictPtr dict, const xmlChar * name, int len);
const xmlChar *xmlDictQLookup (xmlDictPtr dict,
			       const xmlChar * prefix, const xmlChar * name);
int xmlDictOwns (xmlDictPtr dict, const xmlChar * str);
int xmlDictSize (xmlDictPtr dict);
void xmlDictCleanup (void);
xmlRegexpPtr xmlRegexpCompile (const xmlChar * regexp);
void xmlRegFreeRegexp (xmlRegexpPtr regexp);
int xmlRegexpExec (xmlRegexpPtr comp, const xmlChar * value);
void xmlRegexpPrint (FILE * output, xmlRegexpPtr regexp);
int xmlRegexpIsDeterminist (xmlRegexpPtr comp);
typedef void (*xmlRegExecCallbacks) (xmlRegExecCtxtPtr exec,
				     const xmlChar * token,
				     void *transdata, void *inputdata);
xmlRegExecCtxtPtr
xmlRegNewExecCtxt (xmlRegexpPtr comp,
		   xmlRegExecCallbacks callback, void *data);
void xmlRegFreeExecCtxt (xmlRegExecCtxtPtr exec);
int xmlRegExecPushString (xmlRegExecCtxtPtr exec,
			  const xmlChar * value, void *data);
int xmlRegExecPushString2 (xmlRegExecCtxtPtr exec,
			   const xmlChar * value,
			   const xmlChar * value2, void *data);

int xmlRegExecNextValues (xmlRegExecCtxtPtr exec,
			  int *nbval,
			  int *nbneg, xmlChar ** values, int *terminal);
int xmlRegExecErrInfo (xmlRegExecCtxtPtr exec,
		       const xmlChar ** string,
		       int *nbval, int *nbneg, xmlChar ** values, int *terminal);

typedef struct _xmlExpCtxt xmlExpCtxt;
typedef xmlExpCtxt *xmlExpCtxtPtr;
void xmlExpFreeCtxt (xmlExpCtxtPtr ctxt);
xmlExpCtxtPtr xmlExpNewCtxt (int maxNodes, xmlDictPtr dict);
int xmlExpCtxtNbNodes (xmlExpCtxtPtr ctxt);
int xmlExpCtxtNbCons (xmlExpCtxtPtr ctxt);
typedef struct _xmlExpNode xmlExpNode;
typedef xmlExpNode *xmlExpNodePtr;

typedef enum
{
  XML_EXP_EMPTY = 0,
  XML_EXP_FORBID = 1,
  XML_EXP_ATOM = 2,
  XML_EXP_SEQ = 3,
  XML_EXP_OR = 4,
  XML_EXP_COUNT = 5
} xmlExpNodeType;

extern xmlExpNodePtr forbiddenExp;
extern xmlExpNodePtr emptyExp;

void xmlExpFree (xmlExpCtxtPtr ctxt, xmlExpNodePtr expr);
void xmlExpRef (xmlExpNodePtr expr);

xmlExpNodePtr xmlExpParse (xmlExpCtxtPtr ctxt, const char *expr);
xmlExpNodePtr xmlExpNewAtom (xmlExpCtxtPtr ctxt, const xmlChar * name, int len);
xmlExpNodePtr xmlExpNewOr (xmlExpCtxtPtr ctxt, xmlExpNodePtr left, xmlExpNodePtr right);
xmlExpNodePtr xmlExpNewSeq (xmlExpCtxtPtr ctxt, xmlExpNodePtr left, xmlExpNodePtr right);
xmlExpNodePtr xmlExpNewRange (xmlExpCtxtPtr ctxt, xmlExpNodePtr subset, int min, int max);

int xmlExpIsNillable (xmlExpNodePtr expr);
int xmlExpMaxToken (xmlExpNodePtr expr);
int xmlExpGetLanguage (xmlExpCtxtPtr ctxt,
		       xmlExpNodePtr expr, const xmlChar ** langList, int len);
int xmlExpGetStart (xmlExpCtxtPtr ctxt,
		    xmlExpNodePtr expr, const xmlChar ** tokList, int len);
xmlExpNodePtr xmlExpStringDerive (xmlExpCtxtPtr ctxt,
				  xmlExpNodePtr expr, const xmlChar * str, int len);
xmlExpNodePtr xmlExpExpDerive (xmlExpCtxtPtr ctxt, xmlExpNodePtr expr, xmlExpNodePtr sub);
int xmlExpSubsume (xmlExpCtxtPtr ctxt, xmlExpNodePtr expr, xmlExpNodePtr sub);
void xmlExpDump (xmlBufferPtr buf, xmlExpNodePtr expr);
typedef struct _xmlElement xmlElement;
typedef xmlElement *xmlElementPtr;

struct _xmlElement
{
  void *_private;
  xmlElementType type;
  const xmlChar *name;
  struct _xmlNode *children;
  struct _xmlNode *last;
  struct _xmlDtd *parent;
  struct _xmlNode *next;
  struct _xmlNode *prev;
  struct _xmlDoc *doc;
  xmlElementTypeVal etype;
  xmlElementContentPtr content;
  xmlAttributePtr attributes;
  const xmlChar *prefix;
  xmlRegexpPtr contModel;
};

typedef xmlElementType xmlNsType;
typedef struct _xmlNs xmlNs;
typedef xmlNs *xmlNsPtr;

struct _xmlNs
{
  struct _xmlNs *next;
  xmlNsType type;
  const xmlChar *href;
  const xmlChar *prefix;
  void *_private;
  struct _xmlDoc *context;
};

typedef struct _xmlDtd xmlDtd;
typedef xmlDtd *xmlDtdPtr;
struct _xmlDtd
{
  void *_private;
  xmlElementType type;
  const xmlChar *name;
  struct _xmlNode *children;
  struct _xmlNode *last;
  struct _xmlDoc *parent;
  struct _xmlNode *next;
  struct _xmlNode *prev;
  struct _xmlDoc *doc;
  void *notations;
  void *elements;
  void *attributes;
  void *entities;
  const xmlChar *ExternalID;
  const xmlChar *SystemID;
  void *pentities;
};

typedef struct _xmlAttr xmlAttr;
typedef xmlAttr *xmlAttrPtr;
struct _xmlAttr
{
  void *_private;
  xmlElementType type;
  const xmlChar *name;
  struct _xmlNode *children;
  struct _xmlNode *last;
  struct _xmlNode *parent;
  struct _xmlAttr *next;
  struct _xmlAttr *prev;
  struct _xmlDoc *doc;
  xmlNs *ns;
  xmlAttributeType atype;
  void *psvi;
};

typedef struct _xmlID xmlID;
typedef xmlID *xmlIDPtr;
struct _xmlID
{
  struct _xmlID *next;
  const xmlChar *value;
  xmlAttrPtr attr;
  const xmlChar *name;
  int lineno;
  struct _xmlDoc *doc;
};

typedef struct _xmlRef xmlRef;
typedef xmlRef *xmlRefPtr;
struct _xmlRef
{
  struct _xmlRef *next;
  const xmlChar *value;
  xmlAttrPtr attr;
  const xmlChar *name;
  int lineno;
};

typedef struct _xmlNode xmlNode;
typedef xmlNode *xmlNodePtr;
struct _xmlNode
{
  void *_private;
  xmlElementType type;
  const xmlChar *name;
  struct _xmlNode *children;
  struct _xmlNode *last;
  struct _xmlNode *parent;
  struct _xmlNode *next;
  struct _xmlNode *prev;
  struct _xmlDoc *doc;
  xmlNs *ns;
  xmlChar *content;
  struct _xmlAttr *properties;
  xmlNs *nsDef;
  void *psvi;
  unsigned short line;
  unsigned short extra;
};

typedef enum
{
  XML_DOC_WELLFORMED = 1 << 0,
  XML_DOC_NSVALID = 1 << 1,
  XML_DOC_OLD10 = 1 << 2,
  XML_DOC_DTDVALID = 1 << 3,
  XML_DOC_XINCLUDE = 1 << 4,
  XML_DOC_USERBUILT = 1 << 5,
  XML_DOC_INTERNAL = 1 << 6,
  XML_DOC_HTML = 1 << 7
} xmlDocProperties;

typedef struct _xmlDoc xmlDoc;
typedef xmlDoc *xmlDocPtr;

struct _xmlDoc
{
  void *_private;
  xmlElementType type;
  char *name;
  struct _xmlNode *children;
  struct _xmlNode *last;
  struct _xmlNode *parent;
  struct _xmlNode *next;
  struct _xmlNode *prev;
  struct _xmlDoc *doc;
  int compression;
  int standalone;
  struct _xmlDtd *intSubset;
  struct _xmlDtd *extSubset;
  struct _xmlNs *oldNs;
  const xmlChar *version;
  const xmlChar *encoding;
  void *ids;
  void *refs;
  const xmlChar *URL;
  int charset;
  struct _xmlDict *dict;
  void *psvi;
  int parseFlags;
  int properties;
};

typedef struct _xmlDOMWrapCtxt xmlDOMWrapCtxt;
typedef xmlDOMWrapCtxt *xmlDOMWrapCtxtPtr;
typedef xmlNsPtr (*xmlDOMWrapAcquireNsFunction) (xmlDOMWrapCtxtPtr ctxt,
						 xmlNodePtr node,
						 const xmlChar * nsName,
						 const xmlChar * nsPrefix);

struct _xmlDOMWrapCtxt
{
  void *_private;
  int type;
  void *namespaceMap;
  xmlDOMWrapAcquireNsFunction getNsForNodeFunc;
};

int xmlValidateNCName (const xmlChar * value, int space);
int xmlValidateQName (const xmlChar * value, int space);
int xmlValidateName (const xmlChar * value, int space);
int xmlValidateNMToken (const xmlChar * value, int space);
xmlChar *xmlBuildQName (const xmlChar * ncname,
			const xmlChar * prefix, xmlChar * memory, int len);
xmlChar *xmlSplitQName2 (const xmlChar * name, xmlChar ** prefix);
const xmlChar *xmlSplitQName3 (const xmlChar * name, int *len);
void xmlSetBufferAllocationScheme (xmlBufferAllocationScheme scheme);
xmlBufferAllocationScheme xmlGetBufferAllocationScheme (void);
xmlBufferPtr xmlBufferCreate (void);
xmlBufferPtr xmlBufferCreateSize (size_t size);
xmlBufferPtr xmlBufferCreateStatic (void *mem, size_t size);
int xmlBufferResize (xmlBufferPtr buf, unsigned int size);
void xmlBufferFree (xmlBufferPtr buf);
int xmlBufferDump (FILE * file, xmlBufferPtr buf);
int xmlBufferAdd (xmlBufferPtr buf, const xmlChar * str, int len);
int xmlBufferAddHead (xmlBufferPtr buf, const xmlChar * str, int len);
int xmlBufferCat (xmlBufferPtr buf, const xmlChar * str);
int xmlBufferCCat (xmlBufferPtr buf, const char *str);
int xmlBufferShrink (xmlBufferPtr buf, unsigned int len);
int xmlBufferGrow (xmlBufferPtr buf, unsigned int len);
void xmlBufferEmpty (xmlBufferPtr buf);
const xmlChar *xmlBufferContent (const xmlBuffer * buf);
xmlChar *xmlBufferDetach (xmlBufferPtr buf);
void xmlBufferSetAllocationScheme (xmlBufferPtr buf,
				   xmlBufferAllocationScheme scheme);
int xmlBufferLength (const xmlBuffer * buf);

xmlDtdPtr xmlCreateIntSubset (xmlDocPtr doc,
			      const xmlChar * name,
			      const xmlChar * ExternalID, const xmlChar * SystemID);
xmlDtdPtr xmlNewDtd (xmlDocPtr doc,
		     const xmlChar * name,
		     const xmlChar * ExternalID, const xmlChar * SystemID);
xmlDtdPtr xmlGetIntSubset (const xmlDoc * doc);
void xmlFreeDtd (xmlDtdPtr cur);

xmlNsPtr xmlNewGlobalNs (xmlDocPtr doc, const xmlChar * href, const xmlChar * prefix);

xmlNsPtr xmlNewNs (xmlNodePtr node, const xmlChar * href, const xmlChar * prefix);
void xmlFreeNs (xmlNsPtr cur);
void xmlFreeNsList (xmlNsPtr cur);
xmlDocPtr xmlNewDoc (const xmlChar * version);
void xmlFreeDoc (xmlDocPtr cur);
xmlAttrPtr xmlNewDocProp (xmlDocPtr doc, const xmlChar * name, const xmlChar * value);


xmlAttrPtr xmlNewProp (xmlNodePtr node, const xmlChar * name, const xmlChar * value);

xmlAttrPtr xmlNewNsProp (xmlNodePtr node,
			 xmlNsPtr ns, const xmlChar * name, const xmlChar * value);
xmlAttrPtr xmlNewNsPropEatName (xmlNodePtr node,
				xmlNsPtr ns, xmlChar * name, const xmlChar * value);
void xmlFreePropList (xmlAttrPtr cur);
void xmlFreeProp (xmlAttrPtr cur);
xmlAttrPtr xmlCopyProp (xmlNodePtr target, xmlAttrPtr cur);
xmlAttrPtr xmlCopyPropList (xmlNodePtr target, xmlAttrPtr cur);
xmlDtdPtr xmlCopyDtd (xmlDtdPtr dtd);
xmlDocPtr xmlCopyDoc (xmlDocPtr doc, int recursive);
xmlNodePtr xmlNewDocNode (xmlDocPtr doc,
			  xmlNsPtr ns, const xmlChar * name, const xmlChar * content);
xmlNodePtr xmlNewDocNodeEatName (xmlDocPtr doc,
				 xmlNsPtr ns, xmlChar * name, const xmlChar * content);
xmlNodePtr xmlNewNode (xmlNsPtr ns, const xmlChar * name);
xmlNodePtr xmlNewNodeEatName (xmlNsPtr ns, xmlChar * name);

xmlNodePtr xmlNewChild (xmlNodePtr parent,
			xmlNsPtr ns, const xmlChar * name, const xmlChar * content);
xmlNodePtr xmlNewDocText (const xmlDoc * doc, const xmlChar * content);
xmlNodePtr xmlNewText (const xmlChar * content);
xmlNodePtr xmlNewDocPI (xmlDocPtr doc, const xmlChar * name, const xmlChar * content);
xmlNodePtr xmlNewPI (const xmlChar * name, const xmlChar * content);
xmlNodePtr xmlNewDocTextLen (xmlDocPtr doc, const xmlChar * content, int len);
xmlNodePtr xmlNewTextLen (const xmlChar * content, int len);
xmlNodePtr xmlNewDocComment (xmlDocPtr doc, const xmlChar * content);
xmlNodePtr xmlNewComment (const xmlChar * content);
xmlNodePtr xmlNewCDataBlock (xmlDocPtr doc, const xmlChar * content, int len);
xmlNodePtr xmlNewCharRef (xmlDocPtr doc, const xmlChar * name);
xmlNodePtr xmlNewReference (const xmlDoc * doc, const xmlChar * name);
xmlNodePtr xmlCopyNode (xmlNodePtr node, int recursive);
xmlNodePtr xmlDocCopyNode (xmlNodePtr node, xmlDocPtr doc, int recursive);
xmlNodePtr xmlDocCopyNodeList (xmlDocPtr doc, xmlNodePtr node);
xmlNodePtr xmlCopyNodeList (xmlNodePtr node);

xmlNodePtr xmlNewTextChild (xmlNodePtr parent,
			    xmlNsPtr ns, const xmlChar * name, const xmlChar * content);
xmlNodePtr xmlNewDocRawNode (xmlDocPtr doc,
			     xmlNsPtr ns, const xmlChar * name, const xmlChar * content);
xmlNodePtr xmlNewDocFragment (xmlDocPtr doc);
long xmlGetLineNo (const xmlNode * node);
xmlChar *xmlGetNodePath (const xmlNode * node);
xmlNodePtr xmlDocGetRootElement (const xmlDoc * doc);
xmlNodePtr xmlGetLastChild (const xmlNode * parent);
int xmlNodeIsText (const xmlNode * node);
int xmlIsBlankNode (const xmlNode * node);
xmlNodePtr xmlDocSetRootElement (xmlDocPtr doc, xmlNodePtr root);
void xmlNodeSetName (xmlNodePtr cur, const xmlChar * name);
xmlNodePtr xmlAddChild (xmlNodePtr parent, xmlNodePtr cur);
xmlNodePtr xmlAddChildList (xmlNodePtr parent, xmlNodePtr cur);
xmlNodePtr xmlReplaceNode (xmlNodePtr old, xmlNodePtr cur);
xmlNodePtr xmlAddPrevSibling (xmlNodePtr cur, xmlNodePtr elem);
xmlNodePtr xmlAddSibling (xmlNodePtr cur, xmlNodePtr elem);
xmlNodePtr xmlAddNextSibling (xmlNodePtr cur, xmlNodePtr elem);
void xmlUnlinkNode (xmlNodePtr cur);
xmlNodePtr xmlTextMerge (xmlNodePtr first, xmlNodePtr second);
int xmlTextConcat (xmlNodePtr node, const xmlChar * content, int len);
void xmlFreeNodeList (xmlNodePtr cur);
void xmlFreeNode (xmlNodePtr cur);
void xmlSetTreeDoc (xmlNodePtr tree, xmlDocPtr doc);
void xmlSetListDoc (xmlNodePtr list, xmlDocPtr doc);

xmlNsPtr xmlSearchNs (xmlDocPtr doc, xmlNodePtr node, const xmlChar * nameSpace);
xmlNsPtr xmlSearchNsByHref (xmlDocPtr doc, xmlNodePtr node, const xmlChar * href);
xmlNsPtr *xmlGetNsList (const xmlDoc * doc, const xmlNode * node);
void xmlSetNs (xmlNodePtr node, xmlNsPtr ns);
xmlNsPtr xmlCopyNamespace (xmlNsPtr cur);
xmlNsPtr xmlCopyNamespaceList (xmlNsPtr cur);
xmlAttrPtr xmlSetProp (xmlNodePtr node, const xmlChar * name, const xmlChar * value);
xmlAttrPtr xmlSetNsProp (xmlNodePtr node,
			 xmlNsPtr ns, const xmlChar * name, const xmlChar * value);

xmlChar *xmlGetNoNsProp (const xmlNode * node, const xmlChar * name);
xmlChar *xmlGetProp (const xmlNode * node, const xmlChar * name);
xmlAttrPtr xmlHasProp (const xmlNode * node, const xmlChar * name);
xmlAttrPtr xmlHasNsProp (const xmlNode * node,
			 const xmlChar * name, const xmlChar * nameSpace);
xmlChar *xmlGetNsProp (const xmlNode * node,
		       const xmlChar * name, const xmlChar * nameSpace);
xmlNodePtr xmlStringGetNodeList (const xmlDoc * doc, const xmlChar * value);
xmlNodePtr xmlStringLenGetNodeList (const xmlDoc * doc, const xmlChar * value, int len);
xmlChar *xmlNodeListGetString (xmlDocPtr doc,
			       const xmlNode * list, int inLine);

xmlChar *xmlNodeListGetRawString (const xmlDoc * doc,
				  const xmlNode * list, int inLine);

void xmlNodeSetContent (xmlNodePtr cur, const xmlChar * content);
void xmlNodeSetContentLen (xmlNodePtr cur, const xmlChar * content, int len);
void xmlNodeAddContent (xmlNodePtr cur, const xmlChar * content);
void xmlNodeAddContentLen (xmlNodePtr cur, const xmlChar * content, int len);
xmlChar *xmlNodeGetContent (const xmlNode * cur);
int xmlNodeBufGetContent (xmlBufferPtr buffer, const xmlNode * cur);
int xmlBufGetNodeContent (xmlBufPtr buf, const xmlNode * cur);
xmlChar *xmlNodeGetLang (const xmlNode * cur);
int xmlNodeGetSpacePreserve (const xmlNode * cur);
void xmlNodeSetLang (xmlNodePtr cur, const xmlChar * lang);
void xmlNodeSetSpacePreserve (xmlNodePtr cur, int val);
xmlChar *xmlNodeGetBase (const xmlDoc * doc, const xmlNode * cur);
void xmlNodeSetBase (xmlNodePtr cur, const xmlChar * uri);
int xmlRemoveProp (xmlAttrPtr cur);
int xmlUnsetNsProp (xmlNodePtr node, xmlNsPtr ns, const xmlChar * name);
int xmlUnsetProp (xmlNodePtr node, const xmlChar * name);
void xmlBufferWriteCHAR (xmlBufferPtr buf, const xmlChar * string);
void xmlBufferWriteChar (xmlBufferPtr buf, const char *string);
void xmlBufferWriteQuotedString (xmlBufferPtr buf, const xmlChar * string);
void xmlAttrSerializeTxtContent (xmlBufferPtr buf,
				 xmlDocPtr doc,
				 xmlAttrPtr attr, const xmlChar * string);
int xmlReconciliateNs (xmlDocPtr doc, xmlNodePtr tree);
void xmlDocDumpFormatMemory (xmlDocPtr cur, xmlChar ** mem, int *size, int format);
void xmlDocDumpMemory (xmlDocPtr cur, xmlChar ** mem, int *size);
void xmlDocDumpMemoryEnc (xmlDocPtr out_doc,
			  xmlChar ** doc_txt_ptr,
			  int *doc_txt_len, const char *txt_encoding);
void xmlDocDumpFormatMemoryEnc (xmlDocPtr out_doc,
				xmlChar ** doc_txt_ptr,
				int *doc_txt_len,
				const char *txt_encoding, int format);
int xmlDocFormatDump (FILE * f, xmlDocPtr cur, int format);
int xmlDocDump (FILE * f, xmlDocPtr cur);
void xmlElemDump (FILE * f, xmlDocPtr doc, xmlNodePtr cur);
int xmlSaveFile (const char *filename, xmlDocPtr cur);
int xmlSaveFormatFile (const char *filename, xmlDocPtr cur, int format);
size_t xmlBufNodeDump (xmlBufPtr buf,
		       xmlDocPtr doc, xmlNodePtr cur, int level, int format);
int xmlNodeDump (xmlBufferPtr buf,
		 xmlDocPtr doc, xmlNodePtr cur, int level, int format);

int xmlSaveFileTo (xmlOutputBufferPtr buf, xmlDocPtr cur, const char *encoding);
int xmlSaveFormatFileTo (xmlOutputBufferPtr buf,
			 xmlDocPtr cur, const char *encoding, int format);
void xmlNodeDumpOutput (xmlOutputBufferPtr buf,
			xmlDocPtr doc,
			xmlNodePtr cur,
			int level, int format, const char *encoding);

int xmlSaveFormatFileEnc (const char *filename,
			  xmlDocPtr cur, const char *encoding, int format);

int xmlSaveFileEnc (const char *filename, xmlDocPtr cur, const char *encoding);
int xmlIsXHTML (const xmlChar * systemID, const xmlChar * publicID);
int xmlGetDocCompressMode (const xmlDoc * doc);
void xmlSetDocCompressMode (xmlDocPtr doc, int mode);
int xmlGetCompressMode (void);
void xmlSetCompressMode (int mode);
xmlDOMWrapCtxtPtr xmlDOMWrapNewCtxt (void);
void xmlDOMWrapFreeCtxt (xmlDOMWrapCtxtPtr ctxt);
int xmlDOMWrapReconcileNamespaces (xmlDOMWrapCtxtPtr ctxt,
				   xmlNodePtr elem, int options);
int xmlDOMWrapAdoptNode (xmlDOMWrapCtxtPtr ctxt,
			 xmlDocPtr sourceDoc,
			 xmlNodePtr node,
			 xmlDocPtr destDoc, xmlNodePtr destParent, int options);
int xmlDOMWrapRemoveNode (xmlDOMWrapCtxtPtr ctxt,
			  xmlDocPtr doc, xmlNodePtr node, int options);
int xmlDOMWrapCloneNode (xmlDOMWrapCtxtPtr ctxt,
			 xmlDocPtr sourceDoc,
			 xmlNodePtr node,
			 xmlNodePtr * clonedNode,
			 xmlDocPtr destDoc,
			 xmlNodePtr destParent, int deep, int options);

unsigned long xmlChildElementCount (xmlNodePtr parent);
xmlNodePtr xmlNextElementSibling (xmlNodePtr node);
xmlNodePtr xmlFirstElementChild (xmlNodePtr parent);
xmlNodePtr xmlLastElementChild (xmlNodePtr parent);
xmlNodePtr xmlPreviousElementSibling (xmlNodePtr node);
typedef struct _xmlHashTable xmlHashTable;
typedef xmlHashTable *xmlHashTablePtr;
typedef void (*xmlHashDeallocator) (void *payload, xmlChar * name);
typedef void *(*xmlHashCopier) (void *payload, xmlChar * name);
typedef void (*xmlHashScanner) (void *payload, void *data, xmlChar * name);
typedef void (*xmlHashScannerFull) (void *payload, void *data,
				    const xmlChar * name,
				    const xmlChar * name2,
				    const xmlChar * name3);




xmlHashTablePtr xmlHashCreate (int size);
xmlHashTablePtr xmlHashCreateDict (int size, xmlDictPtr dict);
void xmlHashFree (xmlHashTablePtr table, xmlHashDeallocator f);

int xmlHashAddEntry (xmlHashTablePtr table, const xmlChar * name, void *userdata);
int xmlHashUpdateEntry (xmlHashTablePtr table,
			const xmlChar * name,
			void *userdata, xmlHashDeallocator f);
int xmlHashAddEntry2 (xmlHashTablePtr table,
		      const xmlChar * name,
		      const xmlChar * name2, void *userdata);
int xmlHashUpdateEntry2 (xmlHashTablePtr table,
			 const xmlChar * name,
			 const xmlChar * name2,
			 void *userdata, xmlHashDeallocator f);
int xmlHashAddEntry3 (xmlHashTablePtr table,
		      const xmlChar * name,
		      const xmlChar * name2,
		      const xmlChar * name3, void *userdata);
int xmlHashUpdateEntry3 (xmlHashTablePtr table,
			 const xmlChar * name,
			 const xmlChar * name2,
			 const xmlChar * name3,
			 void *userdata, xmlHashDeallocator f);

int xmlHashRemoveEntry (xmlHashTablePtr table, const xmlChar * name,
			xmlHashDeallocator f);
int xmlHashRemoveEntry2 (xmlHashTablePtr table, const xmlChar * name,
			 const xmlChar * name2, xmlHashDeallocator f);
int xmlHashRemoveEntry3 (xmlHashTablePtr table, const xmlChar * name,
			 const xmlChar * name2, const xmlChar * name3,
			 xmlHashDeallocator f);

void *xmlHashLookup (xmlHashTablePtr table, const xmlChar * name);
void *xmlHashLookup2 (xmlHashTablePtr table,
		      const xmlChar * name, const xmlChar * name2);
void *xmlHashLookup3 (xmlHashTablePtr table,
		      const xmlChar * name,
		      const xmlChar * name2, const xmlChar * name3);
void *xmlHashQLookup (xmlHashTablePtr table,
		      const xmlChar * name, const xmlChar * prefix);
void *xmlHashQLookup2 (xmlHashTablePtr table,
		       const xmlChar * name,
		       const xmlChar * prefix,
		       const xmlChar * name2, const xmlChar * prefix2);
void *xmlHashQLookup3 (xmlHashTablePtr table,
		       const xmlChar * name,
		       const xmlChar * prefix,
		       const xmlChar * name2,
		       const xmlChar * prefix2,
		       const xmlChar * name3, const xmlChar * prefix3);

xmlHashTablePtr xmlHashCopy (xmlHashTablePtr table, xmlHashCopier f);
int xmlHashSize (xmlHashTablePtr table);
void xmlHashScan (xmlHashTablePtr table, xmlHashScanner f, void *data);
void xmlHashScan3 (xmlHashTablePtr table,
		   const xmlChar * name,
		   const xmlChar * name2,
		   const xmlChar * name3, xmlHashScanner f, void *data);
void xmlHashScanFull (xmlHashTablePtr table, xmlHashScannerFull f, void *data);
void xmlHashScanFull3 (xmlHashTablePtr table,
		       const xmlChar * name,
		       const xmlChar * name2,
		       const xmlChar * name3, xmlHashScannerFull f, void *data);
typedef enum
{
  XML_ERR_NONE = 0,
  XML_ERR_WARNING = 1,
  XML_ERR_ERROR = 2,
  XML_ERR_FATAL = 3
} xmlErrorLevel;

typedef enum
{
  XML_FROM_NONE = 0,
  XML_FROM_PARSER,
  XML_FROM_TREE,
  XML_FROM_NAMESPACE,
  XML_FROM_DTD,
  XML_FROM_HTML,
  XML_FROM_MEMORY,
  XML_FROM_OUTPUT,
  XML_FROM_IO,
  XML_FROM_FTP,
  XML_FROM_HTTP,
  XML_FROM_XINCLUDE,
  XML_FROM_XPATH,
  XML_FROM_XPOINTER,
  XML_FROM_REGEXP,
  XML_FROM_DATATYPE,
  XML_FROM_SCHEMASP,
  XML_FROM_SCHEMASV,
  XML_FROM_RELAXNGP,
  XML_FROM_RELAXNGV,
  XML_FROM_CATALOG,
  XML_FROM_C14N,
  XML_FROM_XSLT,
  XML_FROM_VALID,
  XML_FROM_CHECK,
  XML_FROM_WRITER,
  XML_FROM_MODULE,
  XML_FROM_I18N,
  XML_FROM_SCHEMATRONV,
  XML_FROM_BUFFER,
  XML_FROM_URI
} xmlErrorDomain;

typedef struct _xmlError xmlError;
typedef xmlError *xmlErrorPtr;
struct _xmlError
{
  int domain;
  int code;
  char *message;
  xmlErrorLevel level;
  char *file;
  int line;
  char *str1;
  char *str2;
  char *str3;
  int int1;
  int int2;
  void *ctxt;
  void *node;
};

typedef enum
{
  XML_ERR_OK = 0,
  XML_ERR_INTERNAL_ERROR,
  XML_ERR_NO_MEMORY,
  XML_ERR_DOCUMENT_START,
  XML_ERR_DOCUMENT_EMPTY,
  XML_ERR_DOCUMENT_END,
  XML_ERR_INVALID_HEX_CHARREF,
  XML_ERR_INVALID_DEC_CHARREF,
  XML_ERR_INVALID_CHARREF,
  XML_ERR_INVALID_CHAR,
  XML_ERR_CHARREF_AT_EOF,
  XML_ERR_CHARREF_IN_PROLOG,
  XML_ERR_CHARREF_IN_EPILOG,
  XML_ERR_CHARREF_IN_DTD,
  XML_ERR_ENTITYREF_AT_EOF,
  XML_ERR_ENTITYREF_IN_PROLOG,
  XML_ERR_ENTITYREF_IN_EPILOG,
  XML_ERR_ENTITYREF_IN_DTD,
  XML_ERR_PEREF_AT_EOF,
  XML_ERR_PEREF_IN_PROLOG,
  XML_ERR_PEREF_IN_EPILOG,
  XML_ERR_PEREF_IN_INT_SUBSET,
  XML_ERR_ENTITYREF_NO_NAME,
  XML_ERR_ENTITYREF_SEMICOL_MISSING,
  XML_ERR_PEREF_NO_NAME,
  XML_ERR_PEREF_SEMICOL_MISSING,
  XML_ERR_UNDECLARED_ENTITY,
  XML_WAR_UNDECLARED_ENTITY,
  XML_ERR_UNPARSED_ENTITY,
  XML_ERR_ENTITY_IS_EXTERNAL,
  XML_ERR_ENTITY_IS_PARAMETER,
  XML_ERR_UNKNOWN_ENCODING,
  XML_ERR_UNSUPPORTED_ENCODING,
  XML_ERR_STRING_NOT_STARTED,
  XML_ERR_STRING_NOT_CLOSED,
  XML_ERR_NS_DECL_ERROR,
  XML_ERR_ENTITY_NOT_STARTED,
  XML_ERR_ENTITY_NOT_FINISHED,
  XML_ERR_LT_IN_ATTRIBUTE,
  XML_ERR_ATTRIBUTE_NOT_STARTED,
  XML_ERR_ATTRIBUTE_NOT_FINISHED,
  XML_ERR_ATTRIBUTE_WITHOUT_VALUE,
  XML_ERR_ATTRIBUTE_REDEFINED,
  XML_ERR_LITERAL_NOT_STARTED,
  XML_ERR_LITERAL_NOT_FINISHED,
  XML_ERR_COMMENT_NOT_FINISHED,
  XML_ERR_PI_NOT_STARTED,
  XML_ERR_PI_NOT_FINISHED,
  XML_ERR_NOTATION_NOT_STARTED,
  XML_ERR_NOTATION_NOT_FINISHED,
  XML_ERR_ATTLIST_NOT_STARTED,
  XML_ERR_ATTLIST_NOT_FINISHED,
  XML_ERR_MIXED_NOT_STARTED,
  XML_ERR_MIXED_NOT_FINISHED,
  XML_ERR_ELEMCONTENT_NOT_STARTED,
  XML_ERR_ELEMCONTENT_NOT_FINISHED,
  XML_ERR_XMLDECL_NOT_STARTED,
  XML_ERR_XMLDECL_NOT_FINISHED,
  XML_ERR_CONDSEC_NOT_STARTED,
  XML_ERR_CONDSEC_NOT_FINISHED,
  XML_ERR_EXT_SUBSET_NOT_FINISHED,
  XML_ERR_DOCTYPE_NOT_FINISHED,
  XML_ERR_MISPLACED_CDATA_END,
  XML_ERR_CDATA_NOT_FINISHED,
  XML_ERR_RESERVED_XML_NAME,
  XML_ERR_SPACE_REQUIRED,
  XML_ERR_SEPARATOR_REQUIRED,
  XML_ERR_NMTOKEN_REQUIRED,
  XML_ERR_NAME_REQUIRED,
  XML_ERR_PCDATA_REQUIRED,
  XML_ERR_URI_REQUIRED,
  XML_ERR_PUBID_REQUIRED,
  XML_ERR_LT_REQUIRED,
  XML_ERR_GT_REQUIRED,
  XML_ERR_LTSLASH_REQUIRED,
  XML_ERR_EQUAL_REQUIRED,
  XML_ERR_TAG_NAME_MISMATCH,
  XML_ERR_TAG_NOT_FINISHED,
  XML_ERR_STANDALONE_VALUE,
  XML_ERR_ENCODING_NAME,
  XML_ERR_HYPHEN_IN_COMMENT,
  XML_ERR_INVALID_ENCODING,
  XML_ERR_EXT_ENTITY_STANDALONE,
  XML_ERR_CONDSEC_INVALID,
  XML_ERR_VALUE_REQUIRED,
  XML_ERR_NOT_WELL_BALANCED,
  XML_ERR_EXTRA_CONTENT,
  XML_ERR_ENTITY_CHAR_ERROR,
  XML_ERR_ENTITY_PE_INTERNAL,
  XML_ERR_ENTITY_LOOP,
  XML_ERR_ENTITY_BOUNDARY,
  XML_ERR_INVALID_URI,
  XML_ERR_URI_FRAGMENT,
  XML_WAR_CATALOG_PI,
  XML_ERR_NO_DTD,
  XML_ERR_CONDSEC_INVALID_KEYWORD,
  XML_ERR_VERSION_MISSING,
  XML_WAR_UNKNOWN_VERSION,
  XML_WAR_LANG_VALUE,
  XML_WAR_NS_URI,
  XML_WAR_NS_URI_RELATIVE,
  XML_ERR_MISSING_ENCODING,
  XML_WAR_SPACE_VALUE,
  XML_ERR_NOT_STANDALONE,
  XML_ERR_ENTITY_PROCESSING,
  XML_ERR_NOTATION_PROCESSING,
  XML_WAR_NS_COLUMN,
  XML_WAR_ENTITY_REDEFINED,
  XML_ERR_UNKNOWN_VERSION,
  XML_ERR_VERSION_MISMATCH,
  XML_ERR_NAME_TOO_LONG,
  XML_ERR_USER_STOP,
  XML_NS_ERR_XML_NAMESPACE = 200,
  XML_NS_ERR_UNDEFINED_NAMESPACE,
  XML_NS_ERR_QNAME,
  XML_NS_ERR_ATTRIBUTE_REDEFINED,
  XML_NS_ERR_EMPTY,
  XML_NS_ERR_COLON,
  XML_DTD_ATTRIBUTE_DEFAULT = 500,
  XML_DTD_ATTRIBUTE_REDEFINED,
  XML_DTD_ATTRIBUTE_VALUE,
  XML_DTD_CONTENT_ERROR,
  XML_DTD_CONTENT_MODEL,
  XML_DTD_CONTENT_NOT_DETERMINIST,
  XML_DTD_DIFFERENT_PREFIX,
  XML_DTD_ELEM_DEFAULT_NAMESPACE,
  XML_DTD_ELEM_NAMESPACE,
  XML_DTD_ELEM_REDEFINED,
  XML_DTD_EMPTY_NOTATION,
  XML_DTD_ENTITY_TYPE,
  XML_DTD_ID_FIXED,
  XML_DTD_ID_REDEFINED,
  XML_DTD_ID_SUBSET,
  XML_DTD_INVALID_CHILD,
  XML_DTD_INVALID_DEFAULT,
  XML_DTD_LOAD_ERROR,
  XML_DTD_MISSING_ATTRIBUTE,
  XML_DTD_MIXED_CORRUPT,
  XML_DTD_MULTIPLE_ID,
  XML_DTD_NO_DOC,
  XML_DTD_NO_DTD,
  XML_DTD_NO_ELEM_NAME,
  XML_DTD_NO_PREFIX,
  XML_DTD_NO_ROOT,
  XML_DTD_NOTATION_REDEFINED,
  XML_DTD_NOTATION_VALUE,
  XML_DTD_NOT_EMPTY,
  XML_DTD_NOT_PCDATA,
  XML_DTD_NOT_STANDALONE,
  XML_DTD_ROOT_NAME,
  XML_DTD_STANDALONE_WHITE_SPACE,
  XML_DTD_UNKNOWN_ATTRIBUTE,
  XML_DTD_UNKNOWN_ELEM,
  XML_DTD_UNKNOWN_ENTITY,
  XML_DTD_UNKNOWN_ID,
  XML_DTD_UNKNOWN_NOTATION,
  XML_DTD_STANDALONE_DEFAULTED,
  XML_DTD_XMLID_VALUE,
  XML_DTD_XMLID_TYPE,
  XML_DTD_DUP_TOKEN,
  XML_HTML_STRUCURE_ERROR = 800,
  XML_HTML_UNKNOWN_TAG,
  XML_RNGP_ANYNAME_ATTR_ANCESTOR = 1000,
  XML_RNGP_ATTR_CONFLICT,
  XML_RNGP_ATTRIBUTE_CHILDREN,
  XML_RNGP_ATTRIBUTE_CONTENT,
  XML_RNGP_ATTRIBUTE_EMPTY,
  XML_RNGP_ATTRIBUTE_NOOP,
  XML_RNGP_CHOICE_CONTENT,
  XML_RNGP_CHOICE_EMPTY,
  XML_RNGP_CREATE_FAILURE,
  XML_RNGP_DATA_CONTENT,
  XML_RNGP_DEF_CHOICE_AND_INTERLEAVE,
  XML_RNGP_DEFINE_CREATE_FAILED,
  XML_RNGP_DEFINE_EMPTY,
  XML_RNGP_DEFINE_MISSING,
  XML_RNGP_DEFINE_NAME_MISSING,
  XML_RNGP_ELEM_CONTENT_EMPTY,
  XML_RNGP_ELEM_CONTENT_ERROR,
  XML_RNGP_ELEMENT_EMPTY,
  XML_RNGP_ELEMENT_CONTENT,
  XML_RNGP_ELEMENT_NAME,
  XML_RNGP_ELEMENT_NO_CONTENT,
  XML_RNGP_ELEM_TEXT_CONFLICT,
  XML_RNGP_EMPTY,
  XML_RNGP_EMPTY_CONSTRUCT,
  XML_RNGP_EMPTY_CONTENT,
  XML_RNGP_EMPTY_NOT_EMPTY,
  XML_RNGP_ERROR_TYPE_LIB,
  XML_RNGP_EXCEPT_EMPTY,
  XML_RNGP_EXCEPT_MISSING,
  XML_RNGP_EXCEPT_MULTIPLE,
  XML_RNGP_EXCEPT_NO_CONTENT,
  XML_RNGP_EXTERNALREF_EMTPY,
  XML_RNGP_EXTERNAL_REF_FAILURE,
  XML_RNGP_EXTERNALREF_RECURSE,
  XML_RNGP_FORBIDDEN_ATTRIBUTE,
  XML_RNGP_FOREIGN_ELEMENT,
  XML_RNGP_GRAMMAR_CONTENT,
  XML_RNGP_GRAMMAR_EMPTY,
  XML_RNGP_GRAMMAR_MISSING,
  XML_RNGP_GRAMMAR_NO_START,
  XML_RNGP_GROUP_ATTR_CONFLICT,
  XML_RNGP_HREF_ERROR,
  XML_RNGP_INCLUDE_EMPTY,
  XML_RNGP_INCLUDE_FAILURE,
  XML_RNGP_INCLUDE_RECURSE,
  XML_RNGP_INTERLEAVE_ADD,
  XML_RNGP_INTERLEAVE_CREATE_FAILED,
  XML_RNGP_INTERLEAVE_EMPTY,
  XML_RNGP_INTERLEAVE_NO_CONTENT,
  XML_RNGP_INVALID_DEFINE_NAME,
  XML_RNGP_INVALID_URI,
  XML_RNGP_INVALID_VALUE,
  XML_RNGP_MISSING_HREF,
  XML_RNGP_NAME_MISSING,
  XML_RNGP_NEED_COMBINE,
  XML_RNGP_NOTALLOWED_NOT_EMPTY,
  XML_RNGP_NSNAME_ATTR_ANCESTOR,
  XML_RNGP_NSNAME_NO_NS,
  XML_RNGP_PARAM_FORBIDDEN,
  XML_RNGP_PARAM_NAME_MISSING,
  XML_RNGP_PARENTREF_CREATE_FAILED,
  XML_RNGP_PARENTREF_NAME_INVALID,
  XML_RNGP_PARENTREF_NO_NAME,
  XML_RNGP_PARENTREF_NO_PARENT,
  XML_RNGP_PARENTREF_NOT_EMPTY,
  XML_RNGP_PARSE_ERROR,
  XML_RNGP_PAT_ANYNAME_EXCEPT_ANYNAME,
  XML_RNGP_PAT_ATTR_ATTR,
  XML_RNGP_PAT_ATTR_ELEM,
  XML_RNGP_PAT_DATA_EXCEPT_ATTR,
  XML_RNGP_PAT_DATA_EXCEPT_ELEM,
  XML_RNGP_PAT_DATA_EXCEPT_EMPTY,
  XML_RNGP_PAT_DATA_EXCEPT_GROUP,
  XML_RNGP_PAT_DATA_EXCEPT_INTERLEAVE,
  XML_RNGP_PAT_DATA_EXCEPT_LIST,
  XML_RNGP_PAT_DATA_EXCEPT_ONEMORE,
  XML_RNGP_PAT_DATA_EXCEPT_REF,
  XML_RNGP_PAT_DATA_EXCEPT_TEXT,
  XML_RNGP_PAT_LIST_ATTR,
  XML_RNGP_PAT_LIST_ELEM,
  XML_RNGP_PAT_LIST_INTERLEAVE,
  XML_RNGP_PAT_LIST_LIST,
  XML_RNGP_PAT_LIST_REF,
  XML_RNGP_PAT_LIST_TEXT,
  XML_RNGP_PAT_NSNAME_EXCEPT_ANYNAME,
  XML_RNGP_PAT_NSNAME_EXCEPT_NSNAME,
  XML_RNGP_PAT_ONEMORE_GROUP_ATTR,
  XML_RNGP_PAT_ONEMORE_INTERLEAVE_ATTR,
  XML_RNGP_PAT_START_ATTR,
  XML_RNGP_PAT_START_DATA,
  XML_RNGP_PAT_START_EMPTY,
  XML_RNGP_PAT_START_GROUP,
  XML_RNGP_PAT_START_INTERLEAVE,
  XML_RNGP_PAT_START_LIST,
  XML_RNGP_PAT_START_ONEMORE,
  XML_RNGP_PAT_START_TEXT,
  XML_RNGP_PAT_START_VALUE,
  XML_RNGP_PREFIX_UNDEFINED,
  XML_RNGP_REF_CREATE_FAILED,
  XML_RNGP_REF_CYCLE,
  XML_RNGP_REF_NAME_INVALID,
  XML_RNGP_REF_NO_DEF,
  XML_RNGP_REF_NO_NAME,
  XML_RNGP_REF_NOT_EMPTY,
  XML_RNGP_START_CHOICE_AND_INTERLEAVE,
  XML_RNGP_START_CONTENT,
  XML_RNGP_START_EMPTY,
  XML_RNGP_START_MISSING,
  XML_RNGP_TEXT_EXPECTED,
  XML_RNGP_TEXT_HAS_CHILD,
  XML_RNGP_TYPE_MISSING,
  XML_RNGP_TYPE_NOT_FOUND,
  XML_RNGP_TYPE_VALUE,
  XML_RNGP_UNKNOWN_ATTRIBUTE,
  XML_RNGP_UNKNOWN_COMBINE,
  XML_RNGP_UNKNOWN_CONSTRUCT,
  XML_RNGP_UNKNOWN_TYPE_LIB,
  XML_RNGP_URI_FRAGMENT,
  XML_RNGP_URI_NOT_ABSOLUTE,
  XML_RNGP_VALUE_EMPTY,
  XML_RNGP_VALUE_NO_CONTENT,
  XML_RNGP_XMLNS_NAME,
  XML_RNGP_XML_NS,
  XML_XPATH_EXPRESSION_OK = 1200,
  XML_XPATH_NUMBER_ERROR,
  XML_XPATH_UNFINISHED_LITERAL_ERROR,
  XML_XPATH_START_LITERAL_ERROR,
  XML_XPATH_VARIABLE_REF_ERROR,
  XML_XPATH_UNDEF_VARIABLE_ERROR,
  XML_XPATH_INVALID_PREDICATE_ERROR,
  XML_XPATH_EXPR_ERROR,
  XML_XPATH_UNCLOSED_ERROR,
  XML_XPATH_UNKNOWN_FUNC_ERROR,
  XML_XPATH_INVALID_OPERAND,
  XML_XPATH_INVALID_TYPE,
  XML_XPATH_INVALID_ARITY,
  XML_XPATH_INVALID_CTXT_SIZE,
  XML_XPATH_INVALID_CTXT_POSITION,
  XML_XPATH_MEMORY_ERROR,
  XML_XPTR_SYNTAX_ERROR,
  XML_XPTR_RESOURCE_ERROR,
  XML_XPTR_SUB_RESOURCE_ERROR,
  XML_XPATH_UNDEF_PREFIX_ERROR,
  XML_XPATH_ENCODING_ERROR,
  XML_XPATH_INVALID_CHAR_ERROR,
  XML_TREE_INVALID_HEX = 1300,
  XML_TREE_INVALID_DEC,
  XML_TREE_UNTERMINATED_ENTITY,
  XML_TREE_NOT_UTF8,
  XML_SAVE_NOT_UTF8 = 1400,
  XML_SAVE_CHAR_INVALID,
  XML_SAVE_NO_DOCTYPE,
  XML_SAVE_UNKNOWN_ENCODING,
  XML_REGEXP_COMPILE_ERROR = 1450,
  XML_IO_UNKNOWN = 1500,
  XML_IO_EACCES,
  XML_IO_EAGAIN,
  XML_IO_EBADF,
  XML_IO_EBADMSG,
  XML_IO_EBUSY,
  XML_IO_ECANCELED,
  XML_IO_ECHILD,
  XML_IO_EDEADLK,
  XML_IO_EDOM,
  XML_IO_EEXIST,
  XML_IO_EFAULT,
  XML_IO_EFBIG,
  XML_IO_EINPROGRESS,
  XML_IO_EINTR,
  XML_IO_EINVAL,
  XML_IO_EIO,
  XML_IO_EISDIR,
  XML_IO_EMFILE,
  XML_IO_EMLINK,
  XML_IO_EMSGSIZE,
  XML_IO_ENAMETOOLONG,
  XML_IO_ENFILE,
  XML_IO_ENODEV,
  XML_IO_ENOENT,
  XML_IO_ENOEXEC,
  XML_IO_ENOLCK,
  XML_IO_ENOMEM,
  XML_IO_ENOSPC,
  XML_IO_ENOSYS,
  XML_IO_ENOTDIR,
  XML_IO_ENOTEMPTY,
  XML_IO_ENOTSUP,
  XML_IO_ENOTTY,
  XML_IO_ENXIO,
  XML_IO_EPERM,
  XML_IO_EPIPE,
  XML_IO_ERANGE,
  XML_IO_EROFS,
  XML_IO_ESPIPE,
  XML_IO_ESRCH,
  XML_IO_ETIMEDOUT,
  XML_IO_EXDEV,
  XML_IO_NETWORK_ATTEMPT,
  XML_IO_ENCODER,
  XML_IO_FLUSH,
  XML_IO_WRITE,
  XML_IO_NO_INPUT,
  XML_IO_BUFFER_FULL,
  XML_IO_LOAD_ERROR,
  XML_IO_ENOTSOCK,
  XML_IO_EISCONN,
  XML_IO_ECONNREFUSED,
  XML_IO_ENETUNREACH,
  XML_IO_EADDRINUSE,
  XML_IO_EALREADY,
  XML_IO_EAFNOSUPPORT,
  XML_XINCLUDE_RECURSION = 1600,
  XML_XINCLUDE_PARSE_VALUE,
  XML_XINCLUDE_ENTITY_DEF_MISMATCH,
  XML_XINCLUDE_NO_HREF,
  XML_XINCLUDE_NO_FALLBACK,
  XML_XINCLUDE_HREF_URI,
  XML_XINCLUDE_TEXT_FRAGMENT,
  XML_XINCLUDE_TEXT_DOCUMENT,
  XML_XINCLUDE_INVALID_CHAR,
  XML_XINCLUDE_BUILD_FAILED,
  XML_XINCLUDE_UNKNOWN_ENCODING,
  XML_XINCLUDE_MULTIPLE_ROOT,
  XML_XINCLUDE_XPTR_FAILED,
  XML_XINCLUDE_XPTR_RESULT,
  XML_XINCLUDE_INCLUDE_IN_INCLUDE,
  XML_XINCLUDE_FALLBACKS_IN_INCLUDE,
  XML_XINCLUDE_FALLBACK_NOT_IN_INCLUDE,
  XML_XINCLUDE_DEPRECATED_NS,
  XML_XINCLUDE_FRAGMENT_ID,
  XML_CATALOG_MISSING_ATTR = 1650,
  XML_CATALOG_ENTRY_BROKEN,
  XML_CATALOG_PREFER_VALUE,
  XML_CATALOG_NOT_CATALOG,
  XML_CATALOG_RECURSION,
  XML_SCHEMAP_PREFIX_UNDEFINED = 1700,
  XML_SCHEMAP_ATTRFORMDEFAULT_VALUE,
  XML_SCHEMAP_ATTRGRP_NONAME_NOREF,
  XML_SCHEMAP_ATTR_NONAME_NOREF,
  XML_SCHEMAP_COMPLEXTYPE_NONAME_NOREF,
  XML_SCHEMAP_ELEMFORMDEFAULT_VALUE,
  XML_SCHEMAP_ELEM_NONAME_NOREF,
  XML_SCHEMAP_EXTENSION_NO_BASE,
  XML_SCHEMAP_FACET_NO_VALUE,
  XML_SCHEMAP_FAILED_BUILD_IMPORT,
  XML_SCHEMAP_GROUP_NONAME_NOREF,
  XML_SCHEMAP_IMPORT_NAMESPACE_NOT_URI,
  XML_SCHEMAP_IMPORT_REDEFINE_NSNAME,
  XML_SCHEMAP_IMPORT_SCHEMA_NOT_URI,
  XML_SCHEMAP_INVALID_BOOLEAN,
  XML_SCHEMAP_INVALID_ENUM,
  XML_SCHEMAP_INVALID_FACET,
  XML_SCHEMAP_INVALID_FACET_VALUE,
  XML_SCHEMAP_INVALID_MAXOCCURS,
  XML_SCHEMAP_INVALID_MINOCCURS,
  XML_SCHEMAP_INVALID_REF_AND_SUBTYPE,
  XML_SCHEMAP_INVALID_WHITE_SPACE,
  XML_SCHEMAP_NOATTR_NOREF,
  XML_SCHEMAP_NOTATION_NO_NAME,
  XML_SCHEMAP_NOTYPE_NOREF,
  XML_SCHEMAP_REF_AND_SUBTYPE,
  XML_SCHEMAP_RESTRICTION_NONAME_NOREF,
  XML_SCHEMAP_SIMPLETYPE_NONAME,
  XML_SCHEMAP_TYPE_AND_SUBTYPE,
  XML_SCHEMAP_UNKNOWN_ALL_CHILD,
  XML_SCHEMAP_UNKNOWN_ANYATTRIBUTE_CHILD,
  XML_SCHEMAP_UNKNOWN_ATTR_CHILD,
  XML_SCHEMAP_UNKNOWN_ATTRGRP_CHILD,
  XML_SCHEMAP_UNKNOWN_ATTRIBUTE_GROUP,
  XML_SCHEMAP_UNKNOWN_BASE_TYPE,
  XML_SCHEMAP_UNKNOWN_CHOICE_CHILD,
  XML_SCHEMAP_UNKNOWN_COMPLEXCONTENT_CHILD,
  XML_SCHEMAP_UNKNOWN_COMPLEXTYPE_CHILD,
  XML_SCHEMAP_UNKNOWN_ELEM_CHILD,
  XML_SCHEMAP_UNKNOWN_EXTENSION_CHILD,
  XML_SCHEMAP_UNKNOWN_FACET_CHILD,
  XML_SCHEMAP_UNKNOWN_FACET_TYPE,
  XML_SCHEMAP_UNKNOWN_GROUP_CHILD,
  XML_SCHEMAP_UNKNOWN_IMPORT_CHILD,
  XML_SCHEMAP_UNKNOWN_LIST_CHILD,
  XML_SCHEMAP_UNKNOWN_NOTATION_CHILD,
  XML_SCHEMAP_UNKNOWN_PROCESSCONTENT_CHILD,
  XML_SCHEMAP_UNKNOWN_REF,
  XML_SCHEMAP_UNKNOWN_RESTRICTION_CHILD,
  XML_SCHEMAP_UNKNOWN_SCHEMAS_CHILD,
  XML_SCHEMAP_UNKNOWN_SEQUENCE_CHILD,
  XML_SCHEMAP_UNKNOWN_SIMPLECONTENT_CHILD,
  XML_SCHEMAP_UNKNOWN_SIMPLETYPE_CHILD,
  XML_SCHEMAP_UNKNOWN_TYPE,
  XML_SCHEMAP_UNKNOWN_UNION_CHILD,
  XML_SCHEMAP_ELEM_DEFAULT_FIXED,
  XML_SCHEMAP_REGEXP_INVALID,
  XML_SCHEMAP_FAILED_LOAD,
  XML_SCHEMAP_NOTHING_TO_PARSE,
  XML_SCHEMAP_NOROOT,
  XML_SCHEMAP_REDEFINED_GROUP,
  XML_SCHEMAP_REDEFINED_TYPE,
  XML_SCHEMAP_REDEFINED_ELEMENT,
  XML_SCHEMAP_REDEFINED_ATTRGROUP,
  XML_SCHEMAP_REDEFINED_ATTR,
  XML_SCHEMAP_REDEFINED_NOTATION,
  XML_SCHEMAP_FAILED_PARSE,
  XML_SCHEMAP_UNKNOWN_PREFIX,
  XML_SCHEMAP_DEF_AND_PREFIX,
  XML_SCHEMAP_UNKNOWN_INCLUDE_CHILD,
  XML_SCHEMAP_INCLUDE_SCHEMA_NOT_URI,
  XML_SCHEMAP_INCLUDE_SCHEMA_NO_URI,
  XML_SCHEMAP_NOT_SCHEMA,
  XML_SCHEMAP_UNKNOWN_MEMBER_TYPE,
  XML_SCHEMAP_INVALID_ATTR_USE,
  XML_SCHEMAP_RECURSIVE,
  XML_SCHEMAP_SUPERNUMEROUS_LIST_ITEM_TYPE,
  XML_SCHEMAP_INVALID_ATTR_COMBINATION,
  XML_SCHEMAP_INVALID_ATTR_INLINE_COMBINATION,
  XML_SCHEMAP_MISSING_SIMPLETYPE_CHILD,
  XML_SCHEMAP_INVALID_ATTR_NAME,
  XML_SCHEMAP_REF_AND_CONTENT,
  XML_SCHEMAP_CT_PROPS_CORRECT_1,
  XML_SCHEMAP_CT_PROPS_CORRECT_2,
  XML_SCHEMAP_CT_PROPS_CORRECT_3,
  XML_SCHEMAP_CT_PROPS_CORRECT_4,
  XML_SCHEMAP_CT_PROPS_CORRECT_5,
  XML_SCHEMAP_DERIVATION_OK_RESTRICTION_1,
  XML_SCHEMAP_DERIVATION_OK_RESTRICTION_2_1_1,
  XML_SCHEMAP_DERIVATION_OK_RESTRICTION_2_1_2,
  XML_SCHEMAP_DERIVATION_OK_RESTRICTION_2_2,
  XML_SCHEMAP_DERIVATION_OK_RESTRICTION_3,
  XML_SCHEMAP_WILDCARD_INVALID_NS_MEMBER,
  XML_SCHEMAP_INTERSECTION_NOT_EXPRESSIBLE,
  XML_SCHEMAP_UNION_NOT_EXPRESSIBLE,
  XML_SCHEMAP_SRC_IMPORT_3_1,
  XML_SCHEMAP_SRC_IMPORT_3_2,
  XML_SCHEMAP_DERIVATION_OK_RESTRICTION_4_1,
  XML_SCHEMAP_DERIVATION_OK_RESTRICTION_4_2,
  XML_SCHEMAP_DERIVATION_OK_RESTRICTION_4_3,
  XML_SCHEMAP_COS_CT_EXTENDS_1_3,
  XML_SCHEMAV_NOROOT = 1801,
  XML_SCHEMAV_UNDECLAREDELEM,
  XML_SCHEMAV_NOTTOPLEVEL,
  XML_SCHEMAV_MISSING,
  XML_SCHEMAV_WRONGELEM,
  XML_SCHEMAV_NOTYPE,
  XML_SCHEMAV_NOROLLBACK,
  XML_SCHEMAV_ISABSTRACT,
  XML_SCHEMAV_NOTEMPTY,
  XML_SCHEMAV_ELEMCONT,
  XML_SCHEMAV_HAVEDEFAULT,
  XML_SCHEMAV_NOTNILLABLE,
  XML_SCHEMAV_EXTRACONTENT,
  XML_SCHEMAV_INVALIDATTR,
  XML_SCHEMAV_INVALIDELEM,
  XML_SCHEMAV_NOTDETERMINIST,
  XML_SCHEMAV_CONSTRUCT,
  XML_SCHEMAV_INTERNAL,
  XML_SCHEMAV_NOTSIMPLE,
  XML_SCHEMAV_ATTRUNKNOWN,
  XML_SCHEMAV_ATTRINVALID,
  XML_SCHEMAV_VALUE,
  XML_SCHEMAV_FACET,
  XML_SCHEMAV_CVC_DATATYPE_VALID_1_2_1,
  XML_SCHEMAV_CVC_DATATYPE_VALID_1_2_2,
  XML_SCHEMAV_CVC_DATATYPE_VALID_1_2_3,
  XML_SCHEMAV_CVC_TYPE_3_1_1,
  XML_SCHEMAV_CVC_TYPE_3_1_2,
  XML_SCHEMAV_CVC_FACET_VALID,
  XML_SCHEMAV_CVC_LENGTH_VALID,
  XML_SCHEMAV_CVC_MINLENGTH_VALID,
  XML_SCHEMAV_CVC_MAXLENGTH_VALID,
  XML_SCHEMAV_CVC_MININCLUSIVE_VALID,
  XML_SCHEMAV_CVC_MAXINCLUSIVE_VALID,
  XML_SCHEMAV_CVC_MINEXCLUSIVE_VALID,
  XML_SCHEMAV_CVC_MAXEXCLUSIVE_VALID,
  XML_SCHEMAV_CVC_TOTALDIGITS_VALID,
  XML_SCHEMAV_CVC_FRACTIONDIGITS_VALID,
  XML_SCHEMAV_CVC_PATTERN_VALID,
  XML_SCHEMAV_CVC_ENUMERATION_VALID,
  XML_SCHEMAV_CVC_COMPLEX_TYPE_2_1,
  XML_SCHEMAV_CVC_COMPLEX_TYPE_2_2,
  XML_SCHEMAV_CVC_COMPLEX_TYPE_2_3,
  XML_SCHEMAV_CVC_COMPLEX_TYPE_2_4,
  XML_SCHEMAV_CVC_ELT_1,
  XML_SCHEMAV_CVC_ELT_2,
  XML_SCHEMAV_CVC_ELT_3_1,
  XML_SCHEMAV_CVC_ELT_3_2_1,
  XML_SCHEMAV_CVC_ELT_3_2_2,
  XML_SCHEMAV_CVC_ELT_4_1,
  XML_SCHEMAV_CVC_ELT_4_2,
  XML_SCHEMAV_CVC_ELT_4_3,
  XML_SCHEMAV_CVC_ELT_5_1_1,
  XML_SCHEMAV_CVC_ELT_5_1_2,
  XML_SCHEMAV_CVC_ELT_5_2_1,
  XML_SCHEMAV_CVC_ELT_5_2_2_1,
  XML_SCHEMAV_CVC_ELT_5_2_2_2_1,
  XML_SCHEMAV_CVC_ELT_5_2_2_2_2,
  XML_SCHEMAV_CVC_ELT_6,
  XML_SCHEMAV_CVC_ELT_7,
  XML_SCHEMAV_CVC_ATTRIBUTE_1,
  XML_SCHEMAV_CVC_ATTRIBUTE_2,
  XML_SCHEMAV_CVC_ATTRIBUTE_3,
  XML_SCHEMAV_CVC_ATTRIBUTE_4,
  XML_SCHEMAV_CVC_COMPLEX_TYPE_3_1,
  XML_SCHEMAV_CVC_COMPLEX_TYPE_3_2_1,
  XML_SCHEMAV_CVC_COMPLEX_TYPE_3_2_2,
  XML_SCHEMAV_CVC_COMPLEX_TYPE_4,
  XML_SCHEMAV_CVC_COMPLEX_TYPE_5_1,
  XML_SCHEMAV_CVC_COMPLEX_TYPE_5_2,
  XML_SCHEMAV_ELEMENT_CONTENT,
  XML_SCHEMAV_DOCUMENT_ELEMENT_MISSING,
  XML_SCHEMAV_CVC_COMPLEX_TYPE_1,
  XML_SCHEMAV_CVC_AU,
  XML_SCHEMAV_CVC_TYPE_1,
  XML_SCHEMAV_CVC_TYPE_2,
  XML_SCHEMAV_CVC_IDC,
  XML_SCHEMAV_CVC_WILDCARD,
  XML_SCHEMAV_MISC,
  XML_XPTR_UNKNOWN_SCHEME = 1900,
  XML_XPTR_CHILDSEQ_START,
  XML_XPTR_EVAL_FAILED,
  XML_XPTR_EXTRA_OBJECTS,
  XML_C14N_CREATE_CTXT = 1950,
  XML_C14N_REQUIRES_UTF8,
  XML_C14N_CREATE_STACK,
  XML_C14N_INVALID_NODE,
  XML_C14N_UNKNOW_NODE,
  XML_C14N_RELATIVE_NAMESPACE,
  XML_FTP_PASV_ANSWER = 2000,
  XML_FTP_EPSV_ANSWER,
  XML_FTP_ACCNT,
  XML_FTP_URL_SYNTAX,
  XML_HTTP_URL_SYNTAX = 2020,
  XML_HTTP_USE_IP,
  XML_HTTP_UNKNOWN_HOST,
  XML_SCHEMAP_SRC_SIMPLE_TYPE_1 = 3000,
  XML_SCHEMAP_SRC_SIMPLE_TYPE_2,
  XML_SCHEMAP_SRC_SIMPLE_TYPE_3,
  XML_SCHEMAP_SRC_SIMPLE_TYPE_4,
  XML_SCHEMAP_SRC_RESOLVE,
  XML_SCHEMAP_SRC_RESTRICTION_BASE_OR_SIMPLETYPE,
  XML_SCHEMAP_SRC_LIST_ITEMTYPE_OR_SIMPLETYPE,
  XML_SCHEMAP_SRC_UNION_MEMBERTYPES_OR_SIMPLETYPES,
  XML_SCHEMAP_ST_PROPS_CORRECT_1,
  XML_SCHEMAP_ST_PROPS_CORRECT_2,
  XML_SCHEMAP_ST_PROPS_CORRECT_3,
  XML_SCHEMAP_COS_ST_RESTRICTS_1_1,
  XML_SCHEMAP_COS_ST_RESTRICTS_1_2,
  XML_SCHEMAP_COS_ST_RESTRICTS_1_3_1,
  XML_SCHEMAP_COS_ST_RESTRICTS_1_3_2,
  XML_SCHEMAP_COS_ST_RESTRICTS_2_1,
  XML_SCHEMAP_COS_ST_RESTRICTS_2_3_1_1,
  XML_SCHEMAP_COS_ST_RESTRICTS_2_3_1_2,
  XML_SCHEMAP_COS_ST_RESTRICTS_2_3_2_1,
  XML_SCHEMAP_COS_ST_RESTRICTS_2_3_2_2,
  XML_SCHEMAP_COS_ST_RESTRICTS_2_3_2_3,
  XML_SCHEMAP_COS_ST_RESTRICTS_2_3_2_4,
  XML_SCHEMAP_COS_ST_RESTRICTS_2_3_2_5,
  XML_SCHEMAP_COS_ST_RESTRICTS_3_1,
  XML_SCHEMAP_COS_ST_RESTRICTS_3_3_1,
  XML_SCHEMAP_COS_ST_RESTRICTS_3_3_1_2,
  XML_SCHEMAP_COS_ST_RESTRICTS_3_3_2_2,
  XML_SCHEMAP_COS_ST_RESTRICTS_3_3_2_1,
  XML_SCHEMAP_COS_ST_RESTRICTS_3_3_2_3,
  XML_SCHEMAP_COS_ST_RESTRICTS_3_3_2_4,
  XML_SCHEMAP_COS_ST_RESTRICTS_3_3_2_5,
  XML_SCHEMAP_COS_ST_DERIVED_OK_2_1,
  XML_SCHEMAP_COS_ST_DERIVED_OK_2_2,
  XML_SCHEMAP_S4S_ELEM_NOT_ALLOWED,
  XML_SCHEMAP_S4S_ELEM_MISSING,
  XML_SCHEMAP_S4S_ATTR_NOT_ALLOWED,
  XML_SCHEMAP_S4S_ATTR_MISSING,
  XML_SCHEMAP_S4S_ATTR_INVALID_VALUE,
  XML_SCHEMAP_SRC_ELEMENT_1,
  XML_SCHEMAP_SRC_ELEMENT_2_1,
  XML_SCHEMAP_SRC_ELEMENT_2_2,
  XML_SCHEMAP_SRC_ELEMENT_3,
  XML_SCHEMAP_P_PROPS_CORRECT_1,
  XML_SCHEMAP_P_PROPS_CORRECT_2_1,
  XML_SCHEMAP_P_PROPS_CORRECT_2_2,
  XML_SCHEMAP_E_PROPS_CORRECT_2,
  XML_SCHEMAP_E_PROPS_CORRECT_3,
  XML_SCHEMAP_E_PROPS_CORRECT_4,
  XML_SCHEMAP_E_PROPS_CORRECT_5,
  XML_SCHEMAP_E_PROPS_CORRECT_6,
  XML_SCHEMAP_SRC_INCLUDE,
  XML_SCHEMAP_SRC_ATTRIBUTE_1,
  XML_SCHEMAP_SRC_ATTRIBUTE_2,
  XML_SCHEMAP_SRC_ATTRIBUTE_3_1,
  XML_SCHEMAP_SRC_ATTRIBUTE_3_2,
  XML_SCHEMAP_SRC_ATTRIBUTE_4,
  XML_SCHEMAP_NO_XMLNS,
  XML_SCHEMAP_NO_XSI,
  XML_SCHEMAP_COS_VALID_DEFAULT_1,
  XML_SCHEMAP_COS_VALID_DEFAULT_2_1,
  XML_SCHEMAP_COS_VALID_DEFAULT_2_2_1,
  XML_SCHEMAP_COS_VALID_DEFAULT_2_2_2,
  XML_SCHEMAP_CVC_SIMPLE_TYPE,
  XML_SCHEMAP_COS_CT_EXTENDS_1_1,
  XML_SCHEMAP_SRC_IMPORT_1_1,
  XML_SCHEMAP_SRC_IMPORT_1_2,
  XML_SCHEMAP_SRC_IMPORT_2,
  XML_SCHEMAP_SRC_IMPORT_2_1,
  XML_SCHEMAP_SRC_IMPORT_2_2,
  XML_SCHEMAP_INTERNAL,
  XML_SCHEMAP_NOT_DETERMINISTIC,
  XML_SCHEMAP_SRC_ATTRIBUTE_GROUP_1,
  XML_SCHEMAP_SRC_ATTRIBUTE_GROUP_2,
  XML_SCHEMAP_SRC_ATTRIBUTE_GROUP_3,
  XML_SCHEMAP_MG_PROPS_CORRECT_1,
  XML_SCHEMAP_MG_PROPS_CORRECT_2,
  XML_SCHEMAP_SRC_CT_1,
  XML_SCHEMAP_DERIVATION_OK_RESTRICTION_2_1_3,
  XML_SCHEMAP_AU_PROPS_CORRECT_2,
  XML_SCHEMAP_A_PROPS_CORRECT_2,
  XML_SCHEMAP_C_PROPS_CORRECT,
  XML_SCHEMAP_SRC_REDEFINE,
  XML_SCHEMAP_SRC_IMPORT,
  XML_SCHEMAP_WARN_SKIP_SCHEMA,
  XML_SCHEMAP_WARN_UNLOCATED_SCHEMA,
  XML_SCHEMAP_WARN_ATTR_REDECL_PROH,
  XML_SCHEMAP_WARN_ATTR_POINTLESS_PROH,
  XML_SCHEMAP_AG_PROPS_CORRECT,
  XML_SCHEMAP_COS_CT_EXTENDS_1_2,
  XML_SCHEMAP_AU_PROPS_CORRECT,
  XML_SCHEMAP_A_PROPS_CORRECT_3,
  XML_SCHEMAP_COS_ALL_LIMITED,
  XML_SCHEMATRONV_ASSERT = 4000,
  XML_SCHEMATRONV_REPORT,
  XML_MODULE_OPEN = 4900,
  XML_MODULE_CLOSE,
  XML_CHECK_FOUND_ELEMENT = 5000,
  XML_CHECK_FOUND_ATTRIBUTE,
  XML_CHECK_FOUND_TEXT,
  XML_CHECK_FOUND_CDATA,
  XML_CHECK_FOUND_ENTITYREF,
  XML_CHECK_FOUND_ENTITY,
  XML_CHECK_FOUND_PI,
  XML_CHECK_FOUND_COMMENT,
  XML_CHECK_FOUND_DOCTYPE,
  XML_CHECK_FOUND_FRAGMENT,
  XML_CHECK_FOUND_NOTATION,
  XML_CHECK_UNKNOWN_NODE,
  XML_CHECK_ENTITY_TYPE,
  XML_CHECK_NO_PARENT,
  XML_CHECK_NO_DOC,
  XML_CHECK_NO_NAME,
  XML_CHECK_NO_ELEM,
  XML_CHECK_WRONG_DOC,
  XML_CHECK_NO_PREV,
  XML_CHECK_WRONG_PREV,
  XML_CHECK_NO_NEXT,
  XML_CHECK_WRONG_NEXT,
  XML_CHECK_NOT_DTD,
  XML_CHECK_NOT_ATTR,
  XML_CHECK_NOT_ATTR_DECL,
  XML_CHECK_NOT_ELEM_DECL,
  XML_CHECK_NOT_ENTITY_DECL,
  XML_CHECK_NOT_NS_DECL,
  XML_CHECK_NO_HREF,
  XML_CHECK_WRONG_PARENT,
  XML_CHECK_NS_SCOPE,
  XML_CHECK_NS_ANCESTOR,
  XML_CHECK_NOT_UTF8,
  XML_CHECK_NO_DICT,
  XML_CHECK_NOT_NCNAME,
  XML_CHECK_OUTSIDE_DICT,
  XML_CHECK_WRONG_NAME,
  XML_CHECK_NAME_NOT_NULL,
  XML_I18N_NO_NAME = 6000,
  XML_I18N_NO_HANDLER,
  XML_I18N_EXCESS_HANDLER,
  XML_I18N_CONV_FAILED,
  XML_I18N_NO_OUTPUT,
  XML_BUF_OVERFLOW = 7000
} xmlParserErrors;

typedef void (*xmlGenericErrorFunc) (void *ctx, const char *msg, ...)
  __attribute__ ((__format__ (__printf__, 2, 3)));
typedef void (*xmlStructuredErrorFunc) (void *userData, xmlErrorPtr error);

void xmlSetGenericErrorFunc (void *ctx, xmlGenericErrorFunc handler);
void initGenericErrorDefaultFunc (xmlGenericErrorFunc * handler);
void xmlSetStructuredErrorFunc (void *ctx, xmlStructuredErrorFunc handler);

void xmlParserError (void *ctx, const char *msg, ...)
  __attribute__ ((__format__ (__printf__, 2, 3)));
void xmlParserWarning (void *ctx, const char *msg, ...)
  __attribute__ ((__format__ (__printf__, 2, 3)));
void xmlParserValidityError (void *ctx, const char *msg, ...)
  __attribute__ ((__format__ (__printf__, 2, 3)));
void xmlParserValidityWarning (void *ctx, const char *msg, ...)
  __attribute__ ((__format__ (__printf__, 2, 3)));
void xmlParserPrintFileInfo (xmlParserInputPtr input);
void xmlParserPrintFileContext (xmlParserInputPtr input);
xmlErrorPtr xmlGetLastError (void);
void xmlResetLastError (void);
xmlErrorPtr xmlCtxtGetLastError (void *ctx);
void xmlCtxtResetLastError (void *ctx);
void xmlResetError (xmlErrorPtr err);
int xmlCopyError (xmlErrorPtr from, xmlErrorPtr to);
typedef struct _xmlLink xmlLink;
typedef xmlLink *xmlLinkPtr;
typedef struct _xmlList xmlList;
typedef xmlList *xmlListPtr;
typedef void (*xmlListDeallocator) (xmlLinkPtr lk);
typedef int (*xmlListDataCompare) (const void *data0, const void *data1);
typedef int (*xmlListWalker) (const void *data, const void *user);

xmlListPtr xmlListCreate (xmlListDeallocator deallocator, xmlListDataCompare compare);
void xmlListDelete (xmlListPtr l);
void *xmlListSearch (xmlListPtr l, void *data);
void *xmlListReverseSearch (xmlListPtr l, void *data);
int xmlListInsert (xmlListPtr l, void *data);
int xmlListAppend (xmlListPtr l, void *data);
int xmlListRemoveFirst (xmlListPtr l, void *data);
int xmlListRemoveLast (xmlListPtr l, void *data);
int xmlListRemoveAll (xmlListPtr l, void *data);
void xmlListClear (xmlListPtr l);
int xmlListEmpty (xmlListPtr l);
xmlLinkPtr xmlListFront (xmlListPtr l);
xmlLinkPtr xmlListEnd (xmlListPtr l);
int xmlListSize (xmlListPtr l);
void xmlListPopFront (xmlListPtr l);
void xmlListPopBack (xmlListPtr l);
int xmlListPushFront (xmlListPtr l, void *data);
int xmlListPushBack (xmlListPtr l, void *data);
void xmlListReverse (xmlListPtr l);
void xmlListSort (xmlListPtr l);
void xmlListWalk (xmlListPtr l, xmlListWalker walker, const void *user);
void xmlListReverseWalk (xmlListPtr l, xmlListWalker walker, const void *user);
void xmlListMerge (xmlListPtr l1, xmlListPtr l2);
xmlListPtr xmlListDup (const xmlListPtr old);
int xmlListCopy (xmlListPtr cur, const xmlListPtr old);
void *xmlLinkGetData (xmlLinkPtr lk);
typedef struct _xmlAutomata xmlAutomata;
typedef xmlAutomata *xmlAutomataPtr;

typedef struct _xmlAutomataState xmlAutomataState;
typedef xmlAutomataState *xmlAutomataStatePtr;

xmlAutomataPtr xmlNewAutomata (void);
void xmlFreeAutomata (xmlAutomataPtr am);

xmlAutomataStatePtr xmlAutomataGetInitState (xmlAutomataPtr am);
int xmlAutomataSetFinalState (xmlAutomataPtr am, xmlAutomataStatePtr state);
xmlAutomataStatePtr xmlAutomataNewState (xmlAutomataPtr am);
xmlAutomataStatePtr
xmlAutomataNewTransition (xmlAutomataPtr am,
			  xmlAutomataStatePtr from,
			  xmlAutomataStatePtr to,
			  const xmlChar * token, void *data);
xmlAutomataStatePtr
xmlAutomataNewTransition2 (xmlAutomataPtr am,
			   xmlAutomataStatePtr from,
			   xmlAutomataStatePtr to,
			   const xmlChar * token,
			   const xmlChar * token2, void *data);
xmlAutomataStatePtr
xmlAutomataNewNegTrans (xmlAutomataPtr am,
			xmlAutomataStatePtr from,
			xmlAutomataStatePtr to,
			const xmlChar * token,
			const xmlChar * token2, void *data);

xmlAutomataStatePtr
xmlAutomataNewCountTrans (xmlAutomataPtr am,
			  xmlAutomataStatePtr from,
			  xmlAutomataStatePtr to,
			  const xmlChar * token,
			  int min, int max, void *data);
xmlAutomataStatePtr
xmlAutomataNewCountTrans2 (xmlAutomataPtr am,
			   xmlAutomataStatePtr from,
			   xmlAutomataStatePtr to,
			   const xmlChar * token,
			   const xmlChar * token2,
			   int min, int max, void *data);
xmlAutomataStatePtr
xmlAutomataNewOnceTrans (xmlAutomataPtr am,
			 xmlAutomataStatePtr from,
			 xmlAutomataStatePtr to,
			 const xmlChar * token, int min, int max, void *data);
xmlAutomataStatePtr
xmlAutomataNewOnceTrans2 (xmlAutomataPtr am,
			  xmlAutomataStatePtr from,
			  xmlAutomataStatePtr to,
			  const xmlChar * token,
			  const xmlChar * token2,
			  int min, int max, void *data);
xmlAutomataStatePtr
xmlAutomataNewAllTrans (xmlAutomataPtr am,
			xmlAutomataStatePtr from,
			xmlAutomataStatePtr to, int lax);
xmlAutomataStatePtr
xmlAutomataNewEpsilon (xmlAutomataPtr am,
		       xmlAutomataStatePtr from, xmlAutomataStatePtr to);
xmlAutomataStatePtr
xmlAutomataNewCountedTrans (xmlAutomataPtr am,
			    xmlAutomataStatePtr from,
			    xmlAutomataStatePtr to, int counter);
xmlAutomataStatePtr
xmlAutomataNewCounterTrans (xmlAutomataPtr am,
			    xmlAutomataStatePtr from,
			    xmlAutomataStatePtr to, int counter);
int xmlAutomataNewCounter (xmlAutomataPtr am, int min, int max);

xmlRegexpPtr xmlAutomataCompile (xmlAutomataPtr am);
int xmlAutomataIsDeterminist (xmlAutomataPtr am);
typedef struct _xmlValidState xmlValidState;
typedef xmlValidState *xmlValidStatePtr;
typedef void (*xmlValidityErrorFunc) (void *ctx, const char *msg, ...)
  __attribute__ ((__format__ (__printf__, 2, 3)));
typedef void (*xmlValidityWarningFunc) (void *ctx, const char *msg, ...)
  __attribute__ ((__format__ (__printf__, 2, 3)));
typedef struct _xmlValidCtxt xmlValidCtxt;
typedef xmlValidCtxt *xmlValidCtxtPtr;

struct _xmlValidCtxt
{
  void *userData;
  xmlValidityErrorFunc error;
  xmlValidityWarningFunc warning;
  xmlNodePtr node;
  int nodeNr;
  int nodeMax;
  xmlNodePtr *nodeTab;
  unsigned int finishDtd;
  xmlDocPtr doc;
  int valid;
  xmlValidState *vstate;
  int vstateNr;
  int vstateMax;
  xmlValidState *vstateTab;
  xmlAutomataPtr am;
  xmlAutomataStatePtr state;
};

typedef struct _xmlHashTable xmlNotationTable;
typedef xmlNotationTable *xmlNotationTablePtr;
typedef struct _xmlHashTable xmlElementTable;
typedef xmlElementTable *xmlElementTablePtr;
typedef struct _xmlHashTable xmlAttributeTable;
typedef xmlAttributeTable *xmlAttributeTablePtr;
typedef struct _xmlHashTable xmlIDTable;
typedef xmlIDTable *xmlIDTablePtr;
typedef struct _xmlHashTable xmlRefTable;
typedef xmlRefTable *xmlRefTablePtr;

xmlNotationPtr xmlAddNotationDecl (xmlValidCtxtPtr ctxt,
				   xmlDtdPtr dtd,
				   const xmlChar * name,
				   const xmlChar * PublicID, const xmlChar * SystemID);

xmlNotationTablePtr xmlCopyNotationTable (xmlNotationTablePtr table);

void xmlFreeNotationTable (xmlNotationTablePtr table);

void xmlDumpNotationDecl (xmlBufferPtr buf, xmlNotationPtr nota);
void xmlDumpNotationTable (xmlBufferPtr buf, xmlNotationTablePtr table);

xmlElementContentPtr xmlNewElementContent (const xmlChar * name, xmlElementContentType type);
xmlElementContentPtr xmlCopyElementContent (xmlElementContentPtr content);
void xmlFreeElementContent (xmlElementContentPtr cur);

xmlElementContentPtr xmlNewDocElementContent (xmlDocPtr doc,
					      const xmlChar * name, xmlElementContentType type);
xmlElementContentPtr xmlCopyDocElementContent (xmlDocPtr doc, xmlElementContentPtr content);
void xmlFreeDocElementContent (xmlDocPtr doc, xmlElementContentPtr cur);
void xmlSnprintfElementContent (char *buf,
				int size,
				xmlElementContentPtr content, int englob);

void xmlSprintfElementContent (char *buf,
			       xmlElementContentPtr content, int englob);

xmlElementPtr xmlAddElementDecl (xmlValidCtxtPtr ctxt,
				 xmlDtdPtr dtd,
				 const xmlChar * name,
				 xmlElementTypeVal type, xmlElementContentPtr content);

xmlElementTablePtr xmlCopyElementTable (xmlElementTablePtr table);

void xmlFreeElementTable (xmlElementTablePtr table);
void xmlDumpElementTable (xmlBufferPtr buf, xmlElementTablePtr table);
void xmlDumpElementDecl (xmlBufferPtr buf, xmlElementPtr elem);
xmlEnumerationPtr xmlCreateEnumeration (const xmlChar * name);
void xmlFreeEnumeration (xmlEnumerationPtr cur);
xmlEnumerationPtr xmlCopyEnumeration (xmlEnumerationPtr cur);

xmlAttributePtr xmlAddAttributeDecl (xmlValidCtxtPtr ctxt,
				     xmlDtdPtr dtd,
				     const xmlChar * elem,
				     const xmlChar * name,
				     const xmlChar * ns,
				     xmlAttributeType type,
				     xmlAttributeDefault def,
				     const xmlChar * defaultValue, xmlEnumerationPtr tree);

xmlAttributeTablePtr xmlCopyAttributeTable (xmlAttributeTablePtr table);
void xmlFreeAttributeTable (xmlAttributeTablePtr table);
void xmlDumpAttributeTable (xmlBufferPtr buf, xmlAttributeTablePtr table);
void xmlDumpAttributeDecl (xmlBufferPtr buf, xmlAttributePtr attr);

xmlIDPtr xmlAddID (xmlValidCtxtPtr ctxt,
		   xmlDocPtr doc, const xmlChar * value, xmlAttrPtr attr);
void xmlFreeIDTable (xmlIDTablePtr table);
xmlAttrPtr xmlGetID (xmlDocPtr doc, const xmlChar * ID);
int xmlIsID (xmlDocPtr doc, xmlNodePtr elem, xmlAttrPtr attr);
int xmlRemoveID (xmlDocPtr doc, xmlAttrPtr attr);

xmlRefPtr xmlAddRef (xmlValidCtxtPtr ctxt,
		     xmlDocPtr doc, const xmlChar * value, xmlAttrPtr attr);
void xmlFreeRefTable (xmlRefTablePtr table);
int xmlIsRef (xmlDocPtr doc, xmlNodePtr elem, xmlAttrPtr attr);
int xmlRemoveRef (xmlDocPtr doc, xmlAttrPtr attr);
xmlListPtr xmlGetRefs (xmlDocPtr doc, const xmlChar * ID);

xmlValidCtxtPtr xmlNewValidCtxt (void);
void xmlFreeValidCtxt (xmlValidCtxtPtr);
int xmlValidateRoot (xmlValidCtxtPtr ctxt, xmlDocPtr doc);
int xmlValidateElementDecl (xmlValidCtxtPtr ctxt,
			    xmlDocPtr doc, xmlElementPtr elem);
xmlChar *xmlValidNormalizeAttributeValue (xmlDocPtr doc,
					  xmlNodePtr elem,
					  const xmlChar * name,
					  const xmlChar * value);
xmlChar *xmlValidCtxtNormalizeAttributeValue (xmlValidCtxtPtr ctxt,
					      xmlDocPtr doc,
					      xmlNodePtr elem,
					      const xmlChar * name,
					      const xmlChar * value);
int xmlValidateAttributeDecl (xmlValidCtxtPtr ctxt,
			      xmlDocPtr doc, xmlAttributePtr attr);
int xmlValidateAttributeValue (xmlAttributeType type, const xmlChar * value);
int xmlValidateNotationDecl (xmlValidCtxtPtr ctxt,
			     xmlDocPtr doc, xmlNotationPtr nota);
int xmlValidateDtd (xmlValidCtxtPtr ctxt, xmlDocPtr doc, xmlDtdPtr dtd);
int xmlValidateDtdFinal (xmlValidCtxtPtr ctxt, xmlDocPtr doc);
int xmlValidateDocument (xmlValidCtxtPtr ctxt, xmlDocPtr doc);
int xmlValidateElement (xmlValidCtxtPtr ctxt, xmlDocPtr doc, xmlNodePtr elem);
int xmlValidateOneElement (xmlValidCtxtPtr ctxt, xmlDocPtr doc, xmlNodePtr elem);
int xmlValidateOneAttribute (xmlValidCtxtPtr ctxt,
			     xmlDocPtr doc,
			     xmlNodePtr elem,
			     xmlAttrPtr attr, const xmlChar * value);
int xmlValidateOneNamespace (xmlValidCtxtPtr ctxt,
			     xmlDocPtr doc,
			     xmlNodePtr elem,
			     const xmlChar * prefix,
			     xmlNsPtr ns, const xmlChar * value);
int xmlValidateDocumentFinal (xmlValidCtxtPtr ctxt, xmlDocPtr doc);



int
xmlValidateNotationUse (xmlValidCtxtPtr ctxt,
			xmlDocPtr doc, const xmlChar * notationName);


int xmlIsMixedElement (xmlDocPtr doc, const xmlChar * name);
xmlAttributePtr xmlGetDtdAttrDesc (xmlDtdPtr dtd, const xmlChar * elem, const xmlChar * name);
xmlAttributePtr xmlGetDtdQAttrDesc (xmlDtdPtr dtd,
				    const xmlChar * elem,
				    const xmlChar * name, const xmlChar * prefix);
xmlNotationPtr xmlGetDtdNotationDesc (xmlDtdPtr dtd, const xmlChar * name);
xmlElementPtr xmlGetDtdQElementDesc (xmlDtdPtr dtd,
				     const xmlChar * name, const xmlChar * prefix);
xmlElementPtr xmlGetDtdElementDesc (xmlDtdPtr dtd, const xmlChar * name);

int xmlValidGetPotentialChildren (xmlElementContent * ctree,
				  const xmlChar ** names, int *len, int max);

int xmlValidGetValidElements (xmlNode * prev,
			      xmlNode * next, const xmlChar ** names, int max);
int xmlValidateNameValue (const xmlChar * value);
int xmlValidateNamesValue (const xmlChar * value);
int xmlValidateNmtokenValue (const xmlChar * value);
int xmlValidateNmtokensValue (const xmlChar * value);

int xmlValidBuildContentModel (xmlValidCtxtPtr ctxt, xmlElementPtr elem);

int
xmlValidatePushElement (xmlValidCtxtPtr ctxt,
			xmlDocPtr doc,
			xmlNodePtr elem, const xmlChar * qname);
int
xmlValidatePushCData (xmlValidCtxtPtr ctxt, const xmlChar * data, int len);
int
xmlValidatePopElement (xmlValidCtxtPtr ctxt,
		       xmlDocPtr doc, xmlNodePtr elem, const xmlChar * qname);
typedef enum
  {
    XML_INTERNAL_GENERAL_ENTITY = 1,
    XML_EXTERNAL_GENERAL_PARSED_ENTITY = 2,
    XML_EXTERNAL_GENERAL_UNPARSED_ENTITY = 3,
    XML_INTERNAL_PARAMETER_ENTITY = 4,
    XML_EXTERNAL_PARAMETER_ENTITY = 5,
    XML_INTERNAL_PREDEFINED_ENTITY = 6
  } xmlEntityType;

struct _xmlEntity
{
  void *_private;
  xmlElementType type;
  const xmlChar *name;
  struct _xmlNode *children;
  struct _xmlNode *last;
  struct _xmlDtd *parent;
  struct _xmlNode *next;
  struct _xmlNode *prev;
  struct _xmlDoc *doc;

  xmlChar *orig;
  xmlChar *content;
  int length;
  xmlEntityType etype;
  const xmlChar *ExternalID;
  const xmlChar *SystemID;

  struct _xmlEntity *nexte;
  const xmlChar *URI;
  int owner;
  int checked;
};

typedef struct _xmlHashTable xmlEntitiesTable;
typedef xmlEntitiesTable *xmlEntitiesTablePtr;
void xmlInitializePredefinedEntities (void);

xmlEntityPtr
xmlNewEntity (xmlDocPtr doc,
	      const xmlChar * name,
	      int type,
	      const xmlChar * ExternalID,
	      const xmlChar * SystemID, const xmlChar * content);
xmlEntityPtr
xmlAddDocEntity (xmlDocPtr doc,
		 const xmlChar * name,
		 int type,
		 const xmlChar * ExternalID,
		 const xmlChar * SystemID, const xmlChar * content);
xmlEntityPtr
xmlAddDtdEntity (xmlDocPtr doc,
		 const xmlChar * name,
		 int type,
		 const xmlChar * ExternalID,
		 const xmlChar * SystemID, const xmlChar * content);
xmlEntityPtr xmlGetPredefinedEntity (const xmlChar * name);
xmlEntityPtr xmlGetDocEntity (const xmlDoc * doc, const xmlChar * name);
xmlEntityPtr xmlGetDtdEntity (xmlDocPtr doc, const xmlChar * name);
xmlEntityPtr xmlGetParameterEntity (xmlDocPtr doc, const xmlChar * name);
const xmlChar *xmlEncodeEntities (xmlDocPtr doc, const xmlChar * input);
xmlChar *xmlEncodeEntitiesReentrant (xmlDocPtr doc, const xmlChar * input);
xmlChar *xmlEncodeSpecialChars (const xmlDoc * doc, const xmlChar * input);
xmlEntitiesTablePtr xmlCreateEntitiesTable (void);

xmlEntitiesTablePtr xmlCopyEntitiesTable (xmlEntitiesTablePtr table);
void xmlFreeEntitiesTable (xmlEntitiesTablePtr table);
void xmlDumpEntitiesTable (xmlBufferPtr buf, xmlEntitiesTablePtr table);
void xmlDumpEntityDecl (xmlBufferPtr buf, xmlEntityPtr ent);
void xmlCleanupPredefinedEntities (void);
typedef void (*xmlParserInputDeallocate) (xmlChar * str);

struct _xmlParserInput
{
  xmlParserInputBufferPtr buf;
  const char *filename;
  const char *directory;
  const xmlChar *base;
  const xmlChar *cur;
  const xmlChar *end;
  int length;
  int line;
  int col;
  unsigned long consumed;
  xmlParserInputDeallocate free;
  const xmlChar *encoding;
  const xmlChar *version;
  int standalone;
  int id;
};

typedef struct _xmlParserNodeInfo xmlParserNodeInfo;
typedef xmlParserNodeInfo *xmlParserNodeInfoPtr;

struct _xmlParserNodeInfo
{
  const struct _xmlNode *node;
  unsigned long begin_pos;
  unsigned long begin_line;
  unsigned long end_pos;
  unsigned long end_line;
};

typedef struct _xmlParserNodeInfoSeq xmlParserNodeInfoSeq;
typedef xmlParserNodeInfoSeq *xmlParserNodeInfoSeqPtr;
struct _xmlParserNodeInfoSeq
{
  unsigned long maximum;
  unsigned long length;
  xmlParserNodeInfo *buffer;
};

typedef enum
  {
    XML_PARSER_EOF = -1,
    XML_PARSER_START = 0,
    XML_PARSER_MISC,
    XML_PARSER_PI,
    XML_PARSER_DTD,
    XML_PARSER_PROLOG,
    XML_PARSER_COMMENT,
    XML_PARSER_START_TAG,
    XML_PARSER_CONTENT,
    XML_PARSER_CDATA_SECTION,
    XML_PARSER_END_TAG,
    XML_PARSER_ENTITY_DECL,
    XML_PARSER_ENTITY_VALUE,
    XML_PARSER_ATTRIBUTE_VALUE,
    XML_PARSER_SYSTEM_LITERAL,
    XML_PARSER_EPILOG,
    XML_PARSER_IGNORE,
    XML_PARSER_PUBLIC_LITERAL
  } xmlParserInputState;

typedef enum
  {
    XML_PARSE_UNKNOWN = 0,
    XML_PARSE_DOM = 1,
    XML_PARSE_SAX = 2,
    XML_PARSE_PUSH_DOM = 3,
    XML_PARSE_PUSH_SAX = 4,
    XML_PARSE_READER = 5
  } xmlParserMode;

struct _xmlParserCtxt
{
  struct _xmlSAXHandler *sax;
  void *userData;
  xmlDocPtr myDoc;
  int wellFormed;
  int replaceEntities;
  const xmlChar *version;
  const xmlChar *encoding;
  int standalone;
  int html;
  xmlParserInputPtr input;
  int inputNr;
  int inputMax;
  xmlParserInputPtr *inputTab;
  xmlNodePtr node;
  int nodeNr;
  int nodeMax;
  xmlNodePtr *nodeTab;
  int record_info;
  xmlParserNodeInfoSeq node_seq;
  int errNo;
  int hasExternalSubset;
  int hasPErefs;
  int external;
  int valid;
  int validate;
  xmlValidCtxt vctxt;
  xmlParserInputState instate;
  int token;
  char *directory;
  const xmlChar *name;
  int nameNr;
  int nameMax;
  const xmlChar **nameTab;
  long nbChars;
  long checkIndex;
  int keepBlanks;
  int disableSAX;
  int inSubset;
  const xmlChar *intSubName;
  xmlChar *extSubURI;
  xmlChar *extSubSystem;
  int *space;
  int spaceNr;
  int spaceMax;
  int *spaceTab;
  int depth;
  xmlParserInputPtr entity;
  int charset;
  int nodelen;
  int nodemem;
  int pedantic;
  void *_private;
  int loadsubset;
  int linenumbers;
  void *catalogs;
  int recovery;
  int progressive;
  xmlDictPtr dict;
  const xmlChar **atts;
  int maxatts;
  int docdict;
  const xmlChar *str_xml;
  const xmlChar *str_xmlns;
  const xmlChar *str_xml_ns;
  int sax2;
  int nsNr;
  int nsMax;
  const xmlChar **nsTab;
  int *attallocs;
  void **pushTab;
  xmlHashTablePtr attsDefault;
  xmlHashTablePtr attsSpecial;
  int nsWellFormed;
  int options;
  int dictNames;
  int freeElemsNr;
  xmlNodePtr freeElems;
  int freeAttrsNr;
  xmlAttrPtr freeAttrs;
  xmlError lastError;
  xmlParserMode parseMode;
  unsigned long nbentities;
  unsigned long sizeentities;
  xmlParserNodeInfo *nodeInfo;
  int nodeInfoNr;
  int nodeInfoMax;
  xmlParserNodeInfo *nodeInfoTab;
  int input_id;
  unsigned long sizeentcopy;
};

struct _xmlSAXLocator
{
  const xmlChar *(*getPublicId) (void *ctx);
  const xmlChar *(*getSystemId) (void *ctx);
  int (*getLineNumber) (void *ctx);
  int (*getColumnNumber) (void *ctx);
};

typedef xmlParserInputPtr (*resolveEntitySAXFunc) (void *ctx,
						   const xmlChar * publicId,
						   const xmlChar * systemId);
typedef void (*internalSubsetSAXFunc) (void *ctx,
				       const xmlChar * name,
				       const xmlChar * ExternalID,
				       const xmlChar * SystemID);
typedef void (*externalSubsetSAXFunc) (void *ctx,
				       const xmlChar * name,
				       const xmlChar * ExternalID,
				       const xmlChar * SystemID);
typedef xmlEntityPtr (*getEntitySAXFunc) (void *ctx, const xmlChar * name);
typedef xmlEntityPtr (*getParameterEntitySAXFunc) (void *ctx,
						   const xmlChar * name);
typedef void (*entityDeclSAXFunc) (void *ctx,
				   const xmlChar * name,
				   int type,
				   const xmlChar * publicId,
				   const xmlChar * systemId,
				   xmlChar * content);
typedef void (*notationDeclSAXFunc) (void *ctx,
				     const xmlChar * name,
				     const xmlChar * publicId,
				     const xmlChar * systemId);
typedef void (*attributeDeclSAXFunc) (void *ctx,
				      const xmlChar * elem,
				      const xmlChar * fullname,
				      int type,
				      int def,
				      const xmlChar * defaultValue,
				      xmlEnumerationPtr tree);
typedef void (*elementDeclSAXFunc) (void *ctx,
				    const xmlChar * name,
				    int type, xmlElementContentPtr content);
typedef void (*unparsedEntityDeclSAXFunc) (void *ctx,
					   const xmlChar * name,
					   const xmlChar * publicId,
					   const xmlChar * systemId,
					   const xmlChar * notationName);
typedef void (*setDocumentLocatorSAXFunc) (void *ctx, xmlSAXLocatorPtr loc);
typedef void (*startDocumentSAXFunc) (void *ctx);
typedef void (*endDocumentSAXFunc) (void *ctx);
typedef void (*startElementSAXFunc) (void *ctx,
				     const xmlChar * name,
				     const xmlChar ** atts);
typedef void (*endElementSAXFunc) (void *ctx, const xmlChar * name);
typedef void (*attributeSAXFunc) (void *ctx,
				  const xmlChar * name,
				  const xmlChar * value);
typedef void (*referenceSAXFunc) (void *ctx, const xmlChar * name);
typedef void (*charactersSAXFunc) (void *ctx, const xmlChar * ch, int len);
typedef void (*ignorableWhitespaceSAXFunc) (void *ctx,
					    const xmlChar * ch, int len);
typedef void (*processingInstructionSAXFunc) (void *ctx,
					      const xmlChar * target,
					      const xmlChar * data);

typedef void (*commentSAXFunc) (void *ctx, const xmlChar * value);
typedef void (*cdataBlockSAXFunc) (void *ctx, const xmlChar * value, int len);
typedef void (*warningSAXFunc) (void *ctx,
				const char *msg, ...)
  __attribute__ ((__format__ (__printf__, 2, 3)));
typedef void (*errorSAXFunc) (void *ctx, const char *msg, ...)
  __attribute__ ((__format__ (__printf__, 2, 3)));
typedef void (*fatalErrorSAXFunc) (void *ctx, const char *msg, ...)
  __attribute__ ((__format__ (__printf__, 2, 3)));
typedef int (*isStandaloneSAXFunc) (void *ctx);
typedef int (*hasInternalSubsetSAXFunc) (void *ctx);
typedef int (*hasExternalSubsetSAXFunc) (void *ctx);
typedef void (*startElementNsSAX2Func) (void *ctx,
					const xmlChar * localname,
					const xmlChar * prefix,
					const xmlChar * URI,
					int nb_namespaces,
					const xmlChar ** namespaces,
					int nb_attributes,
					int nb_defaulted,
					const xmlChar ** attributes);
typedef void (*endElementNsSAX2Func) (void *ctx,
				      const xmlChar * localname,
				      const xmlChar * prefix,
				      const xmlChar * URI);


struct _xmlSAXHandler
{
  internalSubsetSAXFunc internalSubset;
  isStandaloneSAXFunc isStandalone;
  hasInternalSubsetSAXFunc hasInternalSubset;
  hasExternalSubsetSAXFunc hasExternalSubset;
  resolveEntitySAXFunc resolveEntity;
  getEntitySAXFunc getEntity;
  entityDeclSAXFunc entityDecl;
  notationDeclSAXFunc notationDecl;
  attributeDeclSAXFunc attributeDecl;
  elementDeclSAXFunc elementDecl;
  unparsedEntityDeclSAXFunc unparsedEntityDecl;
  setDocumentLocatorSAXFunc setDocumentLocator;
  startDocumentSAXFunc startDocument;
  endDocumentSAXFunc endDocument;
  startElementSAXFunc startElement;
  endElementSAXFunc endElement;
  referenceSAXFunc reference;
  charactersSAXFunc characters;
  ignorableWhitespaceSAXFunc ignorableWhitespace;
  processingInstructionSAXFunc processingInstruction;
  commentSAXFunc comment;
  warningSAXFunc warning;
  errorSAXFunc error;
  fatalErrorSAXFunc fatalError;
  getParameterEntitySAXFunc getParameterEntity;
  cdataBlockSAXFunc cdataBlock;
  externalSubsetSAXFunc externalSubset;
  unsigned int initialized;

  void *_private;
  startElementNsSAX2Func startElementNs;
  endElementNsSAX2Func endElementNs;
  xmlStructuredErrorFunc serror;
};

typedef struct _xmlSAXHandlerV1 xmlSAXHandlerV1;
typedef xmlSAXHandlerV1 *xmlSAXHandlerV1Ptr;
struct _xmlSAXHandlerV1
{
  internalSubsetSAXFunc internalSubset;
  isStandaloneSAXFunc isStandalone;
  hasInternalSubsetSAXFunc hasInternalSubset;
  hasExternalSubsetSAXFunc hasExternalSubset;
  resolveEntitySAXFunc resolveEntity;
  getEntitySAXFunc getEntity;
  entityDeclSAXFunc entityDecl;
  notationDeclSAXFunc notationDecl;
  attributeDeclSAXFunc attributeDecl;
  elementDeclSAXFunc elementDecl;
  unparsedEntityDeclSAXFunc unparsedEntityDecl;
  setDocumentLocatorSAXFunc setDocumentLocator;
  startDocumentSAXFunc startDocument;
  endDocumentSAXFunc endDocument;
  startElementSAXFunc startElement;
  endElementSAXFunc endElement;
  referenceSAXFunc reference;
  charactersSAXFunc characters;
  ignorableWhitespaceSAXFunc ignorableWhitespace;
  processingInstructionSAXFunc processingInstruction;
  commentSAXFunc comment;
  warningSAXFunc warning;
  errorSAXFunc error;
  fatalErrorSAXFunc fatalError;
  getParameterEntitySAXFunc getParameterEntity;
  cdataBlockSAXFunc cdataBlock;
  externalSubsetSAXFunc externalSubset;
  unsigned int initialized;
};
typedef xmlParserInputPtr (*xmlExternalEntityLoader) (const char *URL,
						      const char *ID,
						      xmlParserCtxtPtr
						      context);

typedef void *iconv_t;
extern iconv_t iconv_open (const char *__tocode, const char *__fromcode);
extern size_t iconv (iconv_t __cd, char **__restrict __inbuf,
		     size_t * __restrict __inbytesleft,
		     char **__restrict __outbuf,
		     size_t * __restrict __outbytesleft);

extern int iconv_close (iconv_t __cd);
typedef int8_t UBool;
typedef short unsigned int UChar;
typedef int32_t UChar32;
typedef uint8_t UVersionInfo[4];

extern __attribute__ ((visibility ("default")))
void u_versionFromString_52 (UVersionInfo versionArray,
			     const char *versionString);
extern __attribute__ ((visibility ("default")))
void u_versionFromUString_52 (UVersionInfo versionArray,
			      const UChar * versionString);
extern __attribute__ ((visibility ("default")))
void u_versionToString_52 (const UVersionInfo versionArray,
			   char *versionString);
extern __attribute__ ((visibility ("default")))
void u_getVersion_52 (UVersionInfo versionArray);

extern const uint8_t utf8_countTrailBytes_52[256];
extern __attribute__ ((visibility ("default")))
UChar32 utf8_nextCharSafeBody_52 (const uint8_t * s, int32_t * pi, int32_t length,
				  UChar32 c, UBool strict);
extern __attribute__ ((visibility ("default")))
int32_t utf8_appendCharSafeBody_52 (uint8_t * s, int32_t i, int32_t length,
				    UChar32 c, UBool * pIsError);
extern __attribute__ ((visibility ("default")))
UChar32   utf8_prevCharSafeBody_52 (const uint8_t * s, int32_t start, int32_t * pi,
				    UChar32 c, UBool strict);
extern __attribute__ ((visibility ("default"))) int32_t
utf8_back1SafeBody_52 (const uint8_t * s, int32_t start, int32_t i);

typedef double UDate;

typedef enum UErrorCode {
  U_USING_FALLBACK_WARNING = -128,
  U_ERROR_WARNING_START = -128,
  U_USING_DEFAULT_WARNING = -127,
  U_SAFECLONE_ALLOCATED_WARNING = -126,
  U_STATE_OLD_WARNING = -125,
  U_STRING_NOT_TERMINATED_WARNING = -124,
  U_SORT_KEY_TOO_SHORT_WARNING = -123,
  U_AMBIGUOUS_ALIAS_WARNING = -122,
  U_DIFFERENT_UCA_VERSION = -121,
  U_PLUGIN_CHANGED_LEVEL_WARNING = -120,
  U_ERROR_WARNING_LIMIT,
  U_ZERO_ERROR = 0,
  U_ILLEGAL_ARGUMENT_ERROR = 1,
  U_MISSING_RESOURCE_ERROR = 2,
  U_INVALID_FORMAT_ERROR = 3,
  U_FILE_ACCESS_ERROR = 4,
  U_INTERNAL_PROGRAM_ERROR = 5,
  U_MESSAGE_PARSE_ERROR = 6,
  U_MEMORY_ALLOCATION_ERROR = 7,
  U_INDEX_OUTOFBOUNDS_ERROR = 8,
  U_PARSE_ERROR = 9,
  U_INVALID_CHAR_FOUND = 10,
  U_TRUNCATED_CHAR_FOUND = 11,
  U_ILLEGAL_CHAR_FOUND = 12,
  U_INVALID_TABLE_FORMAT = 13,
  U_INVALID_TABLE_FILE = 14,
  U_BUFFER_OVERFLOW_ERROR = 15,
  U_UNSUPPORTED_ERROR = 16,
  U_RESOURCE_TYPE_MISMATCH = 17,
  U_ILLEGAL_ESCAPE_SEQUENCE = 18,
  U_UNSUPPORTED_ESCAPE_SEQUENCE = 19,
  U_NO_SPACE_AVAILABLE = 20,
  U_CE_NOT_FOUND_ERROR = 21,
  U_PRIMARY_TOO_LONG_ERROR = 22,
  U_STATE_TOO_OLD_ERROR = 23,
  U_TOO_MANY_ALIASES_ERROR = 24,
  U_ENUM_OUT_OF_SYNC_ERROR = 25,
  U_INVARIANT_CONVERSION_ERROR = 26,
  U_INVALID_STATE_ERROR = 27,
  U_COLLATOR_VERSION_MISMATCH = 28,
  U_USELESS_COLLATOR_ERROR = 29,
  U_NO_WRITE_PERMISSION = 30,
  U_STANDARD_ERROR_LIMIT,
  U_BAD_VARIABLE_DEFINITION = 0x10000,
  U_PARSE_ERROR_START = 0x10000,
  U_MALFORMED_RULE,
  U_MALFORMED_SET,
  U_MALFORMED_SYMBOL_REFERENCE,
  U_MALFORMED_UNICODE_ESCAPE,
  U_MALFORMED_VARIABLE_DEFINITION,
  U_MALFORMED_VARIABLE_REFERENCE,
  U_MISMATCHED_SEGMENT_DELIMITERS,
  U_MISPLACED_ANCHOR_START,
  U_MISPLACED_CURSOR_OFFSET,
  U_MISPLACED_QUANTIFIER,
  U_MISSING_OPERATOR,
  U_MISSING_SEGMENT_CLOSE,
  U_MULTIPLE_ANTE_CONTEXTS,
  U_MULTIPLE_CURSORS,
  U_MULTIPLE_POST_CONTEXTS,
  U_TRAILING_BACKSLASH,
  U_UNDEFINED_SEGMENT_REFERENCE,
  U_UNDEFINED_VARIABLE,
  U_UNQUOTED_SPECIAL,
  U_UNTERMINATED_QUOTE,
  U_RULE_MASK_ERROR,
  U_MISPLACED_COMPOUND_FILTER,
  U_MULTIPLE_COMPOUND_FILTERS,
  U_INVALID_RBT_SYNTAX,
  U_INVALID_PROPERTY_PATTERN,
  U_MALFORMED_PRAGMA,
  U_UNCLOSED_SEGMENT,
  U_ILLEGAL_CHAR_IN_SEGMENT,
  U_VARIABLE_RANGE_EXHAUSTED,
  U_VARIABLE_RANGE_OVERLAP,
  U_ILLEGAL_CHARACTER,
  U_INTERNAL_TRANSLITERATOR_ERROR,
  U_INVALID_ID,
  U_INVALID_FUNCTION,
  U_PARSE_ERROR_LIMIT,
  U_UNEXPECTED_TOKEN = 0x10100,
  U_FMT_PARSE_ERROR_START = 0x10100,
  U_MULTIPLE_DECIMAL_SEPARATORS,
  U_MULTIPLE_DECIMAL_SEPERATORS = U_MULTIPLE_DECIMAL_SEPARATORS,
  U_MULTIPLE_EXPONENTIAL_SYMBOLS,
  U_MALFORMED_EXPONENTIAL_PATTERN,
  U_MULTIPLE_PERCENT_SYMBOLS,
  U_MULTIPLE_PERMILL_SYMBOLS,
  U_MULTIPLE_PAD_SPECIFIERS,
  U_PATTERN_SYNTAX_ERROR,
  U_ILLEGAL_PAD_POSITION,
  U_UNMATCHED_BRACES,
  U_UNSUPPORTED_PROPERTY,
  U_UNSUPPORTED_ATTRIBUTE,
  U_ARGUMENT_TYPE_MISMATCH,
  U_DUPLICATE_KEYWORD,
  U_UNDEFINED_KEYWORD,
  U_DEFAULT_KEYWORD_MISSING,
  U_DECIMAL_NUMBER_SYNTAX_ERROR,
  U_FORMAT_INEXACT_ERROR,
  U_FMT_PARSE_ERROR_LIMIT,
  U_BRK_INTERNAL_ERROR = 0x10200,
  U_BRK_ERROR_START = 0x10200,
  U_BRK_HEX_DIGITS_EXPECTED,
  U_BRK_SEMICOLON_EXPECTED,
  U_BRK_RULE_SYNTAX,
  U_BRK_UNCLOSED_SET,
  U_BRK_ASSIGN_ERROR,
  U_BRK_VARIABLE_REDFINITION,
  U_BRK_MISMATCHED_PAREN,
  U_BRK_NEW_LINE_IN_QUOTED_STRING,
  U_BRK_UNDEFINED_VARIABLE,
  U_BRK_INIT_ERROR,
  U_BRK_RULE_EMPTY_SET,
  U_BRK_UNRECOGNIZED_OPTION,
  U_BRK_MALFORMED_RULE_TAG,
  U_BRK_ERROR_LIMIT,
  U_REGEX_INTERNAL_ERROR = 0x10300,
  U_REGEX_ERROR_START = 0x10300,
  U_REGEX_RULE_SYNTAX,
  U_REGEX_INVALID_STATE,
  U_REGEX_BAD_ESCAPE_SEQUENCE,
  U_REGEX_PROPERTY_SYNTAX,
  U_REGEX_UNIMPLEMENTED,
  U_REGEX_MISMATCHED_PAREN,
  U_REGEX_NUMBER_TOO_BIG,
  U_REGEX_BAD_INTERVAL,
  U_REGEX_MAX_LT_MIN,
  U_REGEX_INVALID_BACK_REF,
  U_REGEX_INVALID_FLAG,
  U_REGEX_LOOK_BEHIND_LIMIT,
  U_REGEX_SET_CONTAINS_STRING,
  U_REGEX_OCTAL_TOO_BIG,
  U_REGEX_MISSING_CLOSE_BRACKET,
  U_REGEX_INVALID_RANGE,
  U_REGEX_STACK_OVERFLOW,
  U_REGEX_TIME_OUT,
  U_REGEX_STOPPED_BY_CALLER,
  U_REGEX_PATTERN_TOO_BIG,
  U_REGEX_ERROR_LIMIT,
  U_IDNA_PROHIBITED_ERROR = 0x10400,
  U_IDNA_ERROR_START = 0x10400,
  U_IDNA_UNASSIGNED_ERROR,
  U_IDNA_CHECK_BIDI_ERROR,
  U_IDNA_STD3_ASCII_RULES_ERROR,
  U_IDNA_ACE_PREFIX_ERROR,
  U_IDNA_VERIFICATION_ERROR,
  U_IDNA_LABEL_TOO_LONG_ERROR,
  U_IDNA_ZERO_LENGTH_LABEL_ERROR,
  U_IDNA_DOMAIN_NAME_TOO_LONG_ERROR,
  U_IDNA_ERROR_LIMIT,
  U_STRINGPREP_PROHIBITED_ERROR = U_IDNA_PROHIBITED_ERROR,
  U_STRINGPREP_UNASSIGNED_ERROR = U_IDNA_UNASSIGNED_ERROR,
  U_STRINGPREP_CHECK_BIDI_ERROR = U_IDNA_CHECK_BIDI_ERROR,
  U_PLUGIN_ERROR_START = 0x10500,
  U_PLUGIN_TOO_HIGH = 0x10500,
  U_PLUGIN_DIDNT_SET_LEVEL,
  U_PLUGIN_ERROR_LIMIT,
  U_ERROR_LIMIT = U_PLUGIN_ERROR_LIMIT
} UErrorCode;

extern __attribute__ ((visibility ("default")))
const char * u_errorName_52 (UErrorCode code);

struct UConverter;

typedef struct UConverter UConverter;
typedef enum {
  UCNV_UNASSIGNED = 0,
  UCNV_ILLEGAL = 1,
  UCNV_IRREGULAR = 2,
  UCNV_RESET = 3,
  UCNV_CLOSE = 4,
  UCNV_CLONE = 5
} UConverterCallbackReason;

typedef struct
{
  uint16_t size;
  UBool flush;
  UConverter *converter;
  const UChar *source;
  const UChar *sourceLimit;
  char *target;
  const char *targetLimit;
  int32_t * offsets;
} UConverterFromUnicodeArgs;

typedef struct
{
  uint16_t size;
  UBool flush;
  UConverter *converter;
  const char *source;
  const char *sourceLimit;
  UChar *target;
  const UChar *targetLimit;
  int32_t *offsets;
} UConverterToUnicodeArgs;

extern __attribute__ ((visibility ("default")))
void UCNV_FROM_U_CALLBACK_STOP_52 (const void *context,
				   UConverterFromUnicodeArgs * fromUArgs,
				   const UChar * codeUnits,
				   int32_t length,
				   UChar32 codePoint,
				   UConverterCallbackReason reason,
				   UErrorCode * err);

extern __attribute__ ((visibility ("default")))
void UCNV_TO_U_CALLBACK_STOP_52 (const void *context,
				 UConverterToUnicodeArgs * toUArgs,
				 const char *codeUnits,
				 int32_t length,
				 UConverterCallbackReason reason,
				 UErrorCode * err);

extern __attribute__ ((visibility ("default")))
void UCNV_FROM_U_CALLBACK_SKIP_52 (const void *context,
				   UConverterFromUnicodeArgs * fromUArgs,
				   const UChar * codeUnits,
				   int32_t length,
				   UChar32 codePoint,
				   UConverterCallbackReason reason,
				   UErrorCode * err);

extern __attribute__ ((visibility ("default")))
void UCNV_FROM_U_CALLBACK_SUBSTITUTE_52 (const void *context,
					 UConverterFromUnicodeArgs *
					 fromUArgs, const UChar * codeUnits,
					 int32_t length, UChar32 codePoint,
					 UConverterCallbackReason reason,
					 UErrorCode * err);

extern __attribute__ ((visibility ("default")))
void UCNV_FROM_U_CALLBACK_ESCAPE_52 (const void *context,
				     UConverterFromUnicodeArgs * fromUArgs,
				     const UChar * codeUnits,
				     int32_t length,
				     UChar32 codePoint,
				     UConverterCallbackReason reason,
				     UErrorCode * err);

extern __attribute__ ((visibility ("default")))
void UCNV_TO_U_CALLBACK_SKIP_52 (const void *context,
				 UConverterToUnicodeArgs * toUArgs,
				 const char *codeUnits,
				 int32_t length,
				 UConverterCallbackReason reason,
				 UErrorCode * err);

extern __attribute__ ((visibility ("default")))
void UCNV_TO_U_CALLBACK_SUBSTITUTE_52 (const void *context,
				       UConverterToUnicodeArgs * toUArgs,
				       const char *codeUnits,
				       int32_t length,
				       UConverterCallbackReason reason,
				       UErrorCode * err);

extern __attribute__ ((visibility ("default")))
void UCNV_TO_U_CALLBACK_ESCAPE_52 (const void *context,
				   UConverterToUnicodeArgs * toUArgs,
				   const char *codeUnits,
				   int32_t length,
				   UConverterCallbackReason reason,
				   UErrorCode * err);
struct UEnumeration;

typedef struct UEnumeration UEnumeration;

extern __attribute__ ((visibility ("default")))
void uenum_close_52 (UEnumeration * en);

extern __attribute__ ((visibility ("default")))
int32_t uenum_count_52 (UEnumeration * en, UErrorCode * status);

extern __attribute__ ((visibility ("default")))
const UChar *uenum_unext_52 (UEnumeration * en,
			     int32_t * resultLength, UErrorCode * status);

extern __attribute__ ((visibility ("default")))
const char *uenum_next_52 (UEnumeration * en,
			   int32_t * resultLength, UErrorCode * status);

extern __attribute__ ((visibility ("default")))
void uenum_reset_52 (UEnumeration * en, UErrorCode * status);

extern __attribute__ ((visibility ("default")))
UEnumeration *uenum_openUCharStringsEnumeration_52 (const UChar * const strings[],
						    int32_t count, UErrorCode * ec);

extern __attribute__ ((visibility ("default")))
UEnumeration *uenum_openCharStringsEnumeration_52 (const char *const strings[],
						   int32_t count, UErrorCode * ec);

struct USet;
typedef struct USet USet;

typedef enum {
  UCNV_UNSUPPORTED_CONVERTER = -1,
  UCNV_SBCS = 0,
  UCNV_DBCS = 1,
  UCNV_MBCS = 2,
  UCNV_LATIN_1 = 3,
  UCNV_UTF8 = 4,
  UCNV_UTF16_BigEndian = 5,
  UCNV_UTF16_LittleEndian = 6,
  UCNV_UTF32_BigEndian = 7,
  UCNV_UTF32_LittleEndian = 8,
  UCNV_EBCDIC_STATEFUL = 9,
  UCNV_ISO_2022 = 10,
  UCNV_LMBCS_1 = 11,
  UCNV_LMBCS_2,
  UCNV_LMBCS_3,
  UCNV_LMBCS_4,
  UCNV_LMBCS_5,
  UCNV_LMBCS_6,
  UCNV_LMBCS_8,
  UCNV_LMBCS_11,
  UCNV_LMBCS_16,
  UCNV_LMBCS_17,
  UCNV_LMBCS_18,
  UCNV_LMBCS_19,
  UCNV_LMBCS_LAST = UCNV_LMBCS_19,
  UCNV_HZ,
  UCNV_SCSU,
  UCNV_ISCII,
  UCNV_US_ASCII,
  UCNV_UTF7,
  UCNV_BOCU1,
  UCNV_UTF16,
  UCNV_UTF32,
  UCNV_CESU8,
  UCNV_IMAP_MAILBOX,
  UCNV_COMPOUND_TEXT,
  UCNV_NUMBER_OF_SUPPORTED_CONVERTER_TYPES
} UConverterType;

typedef enum {
  UCNV_UNKNOWN = -1,
  UCNV_IBM = 0
} UConverterPlatform;

typedef void (*UConverterToUCallback) (const void *context,
				       UConverterToUnicodeArgs * args,
				       const char *codeUnits,
				       int32_t length,
				       UConverterCallbackReason reason,
				       UErrorCode * pErrorCode);

typedef void (*UConverterFromUCallback) (const void *context,
					 UConverterFromUnicodeArgs *
					 args, const UChar * codeUnits,
					 int32_t length,
					 UChar32 codePoint,
					 UConverterCallbackReason reason,
					 UErrorCode * pErrorCode);

extern __attribute__ ((visibility ("default")))
int ucnv_compareNames_52 (const char *name1, const char *name2);

extern __attribute__ ((visibility ("default")))
UConverter *ucnv_open_52 (const char *converterName, UErrorCode * err);

extern __attribute__ ((visibility ("default")))
UConverter *ucnv_openU_52 (const UChar * name, UErrorCode * err);

extern __attribute__ ((visibility ("default")))
UConverter *ucnv_openCCSID_52 (int32_t codepage,
			       UConverterPlatform platform, UErrorCode * err);

extern __attribute__ ((visibility ("default")))
UConverter *ucnv_openPackage_52 (const char *packageName, const char *converterName,
				 UErrorCode * err);
extern __attribute__ ((visibility ("default")))
UConverter *
ucnv_safeClone_52 (const UConverter * cnv,
		   void *stackBuffer,
		   int32_t * pBufferSize, UErrorCode * status);
extern __attribute__ ((visibility ("default")))
void
ucnv_close_52 (UConverter * converter);
extern __attribute__ ((visibility ("default")))
void
ucnv_getSubstChars_52 (const UConverter * converter,
		       char *subChars, int8_t * len, UErrorCode * err);
extern __attribute__ ((visibility ("default")))
void
ucnv_setSubstChars_52 (UConverter * converter,
		       const char *subChars,
		       int8_t len, UErrorCode * err);
extern __attribute__ ((visibility ("default")))
void
ucnv_setSubstString_52 (UConverter * cnv,
			const UChar * s,
			int32_t length, UErrorCode * err);
extern __attribute__ ((visibility ("default")))
void
ucnv_getInvalidChars_52 (const UConverter * converter,
			 char *errBytes, int8_t * len, UErrorCode * err);
extern __attribute__ ((visibility ("default")))
void
ucnv_getInvalidUChars_52 (const UConverter * converter,
			  UChar * errUChars,
			  int8_t * len, UErrorCode * err);
extern __attribute__ ((visibility ("default")))
void
ucnv_reset_52 (UConverter * converter);
extern __attribute__ ((visibility ("default")))
void
ucnv_resetToUnicode_52 (UConverter * converter);
extern __attribute__ ((visibility ("default")))
void
ucnv_resetFromUnicode_52 (UConverter * converter);
extern __attribute__ ((visibility ("default"))) int8_t
ucnv_getMaxCharSize_52 (const UConverter * converter);
extern __attribute__ ((visibility ("default"))) int8_t
ucnv_getMinCharSize_52 (const UConverter * converter);
extern __attribute__ ((visibility ("default"))) int32_t
ucnv_getDisplayName_52 (const UConverter * converter,
			const char *displayLocale,
			UChar * displayName,
			int32_t displayNameCapacity, UErrorCode * err);
extern __attribute__ ((visibility ("default")))
const char *
ucnv_getName_52 (const UConverter * converter, UErrorCode * err);
extern __attribute__ ((visibility ("default"))) int32_t
ucnv_getCCSID_52 (const UConverter * converter, UErrorCode * err);
extern __attribute__ ((visibility ("default"))) UConverterPlatform
ucnv_getPlatform_52 (const UConverter * converter, UErrorCode * err);
extern __attribute__ ((visibility ("default"))) UConverterType
ucnv_getType_52 (const UConverter * converter);
extern __attribute__ ((visibility ("default")))
void
ucnv_getStarters_52 (const UConverter * converter,
		     UBool starters[256], UErrorCode * err);

typedef enum UConverterUnicodeSet {

  UCNV_ROUNDTRIP_SET,

  UCNV_ROUNDTRIP_AND_FALLBACK_SET,

  UCNV_SET_COUNT
} UConverterUnicodeSet;
extern __attribute__ ((visibility ("default")))
void
ucnv_getUnicodeSet_52 (const UConverter * cnv,
		       USet * setFillIn,
		       UConverterUnicodeSet whichSet,
		       UErrorCode * pErrorCode);
extern __attribute__ ((visibility ("default")))
void
ucnv_getToUCallBack_52 (const UConverter * converter,
			UConverterToUCallback * action,
			const void **context);
extern __attribute__ ((visibility ("default")))
void
ucnv_getFromUCallBack_52 (const UConverter * converter,
			  UConverterFromUCallback * action,
			  const void **context);
extern __attribute__ ((visibility ("default")))
void
ucnv_setToUCallBack_52 (UConverter * converter,
			UConverterToUCallback newAction,
			const void *newContext,
			UConverterToUCallback * oldAction,
			const void **oldContext, UErrorCode * err);
extern __attribute__ ((visibility ("default")))
void
ucnv_setFromUCallBack_52 (UConverter * converter,
			  UConverterFromUCallback newAction,
			  const void *newContext,
			  UConverterFromUCallback * oldAction,
			  const void **oldContext, UErrorCode * err);
extern __attribute__ ((visibility ("default")))
void
ucnv_fromUnicode_52 (UConverter * converter,
		     char **target,
		     const char *targetLimit,
		     const UChar ** source,
		     const UChar * sourceLimit,
		     int32_t * offsets, UBool flush, UErrorCode * err);
extern __attribute__ ((visibility ("default")))
void
ucnv_toUnicode_52 (UConverter * converter,
		   UChar ** target,
		   const UChar * targetLimit,
		   const char **source,
		   const char *sourceLimit,
		   int32_t * offsets, UBool flush, UErrorCode * err);
extern __attribute__ ((visibility ("default"))) int32_t
ucnv_fromUChars_52 (UConverter * cnv,
		    char *dest, int32_t destCapacity,
		    const UChar * src, int32_t srcLength,
		    UErrorCode * pErrorCode);
extern __attribute__ ((visibility ("default"))) int32_t
ucnv_toUChars_52 (UConverter * cnv,
		  UChar * dest, int32_t destCapacity,
		  const char *src, int32_t srcLength,
		  UErrorCode * pErrorCode);
extern __attribute__ ((visibility ("default"))) UChar32
ucnv_getNextUChar_52 (UConverter * converter,
		      const char **source,
		      const char *sourceLimit, UErrorCode * err);
extern __attribute__ ((visibility ("default")))
void
ucnv_convertEx_52 (UConverter * targetCnv, UConverter * sourceCnv,
		   char **target, const char *targetLimit,
		   const char **source, const char *sourceLimit,
		   UChar * pivotStart, UChar ** pivotSource,
		   UChar ** pivotTarget, const UChar * pivotLimit,
		   UBool reset, UBool flush, UErrorCode * pErrorCode);
extern __attribute__ ((visibility ("default"))) int32_t
ucnv_convert_52 (const char *toConverterName,
		 const char *fromConverterName,
		 char *target,
		 int32_t targetCapacity,
		 const char *source,
		 int32_t sourceLength, UErrorCode * pErrorCode);
extern __attribute__ ((visibility ("default"))) int32_t
ucnv_toAlgorithmic_52 (UConverterType algorithmicType,
		       UConverter * cnv,
		       char *target, int32_t targetCapacity,
		       const char *source, int32_t sourceLength,
		       UErrorCode * pErrorCode);
extern __attribute__ ((visibility ("default"))) int32_t
ucnv_fromAlgorithmic_52 (UConverter * cnv,
			 UConverterType algorithmicType,
			 char *target, int32_t targetCapacity,
			 const char *source, int32_t sourceLength,
			 UErrorCode * pErrorCode);
extern __attribute__ ((visibility ("default"))) int32_t
ucnv_flushCache_52 (void);
extern __attribute__ ((visibility ("default"))) int32_t
ucnv_countAvailable_52 (void);
extern __attribute__ ((visibility ("default")))
const char *
ucnv_getAvailableName_52 (int32_t n);
extern __attribute__ ((visibility ("default")))
UEnumeration *
ucnv_openAllNames_52 (UErrorCode * pErrorCode);
extern __attribute__ ((visibility ("default"))) uint16_t
ucnv_countAliases_52 (const char *alias, UErrorCode * pErrorCode);
extern __attribute__ ((visibility ("default")))
const char *
ucnv_getAlias_52 (const char *alias, uint16_t n,
		  UErrorCode * pErrorCode);
extern __attribute__ ((visibility ("default")))
void
ucnv_getAliases_52 (const char *alias, const char **aliases,
		    UErrorCode * pErrorCode);
extern __attribute__ ((visibility ("default")))
UEnumeration *
ucnv_openStandardNames_52 (const char *convName,
			   const char *standard,
			   UErrorCode * pErrorCode);






extern __attribute__ ((visibility ("default"))) uint16_t
ucnv_countStandards_52 (void);
extern __attribute__ ((visibility ("default")))
const char *
ucnv_getStandard_52 (uint16_t n, UErrorCode * pErrorCode);
extern __attribute__ ((visibility ("default")))
const char *
ucnv_getStandardName_52 (const char *name, const char *standard,
			 UErrorCode * pErrorCode);
extern __attribute__ ((visibility ("default")))
const char *
ucnv_getCanonicalName_52 (const char *alias, const char *standard,
			  UErrorCode * pErrorCode);
extern __attribute__ ((visibility ("default")))
const char *
ucnv_getDefaultName_52 (void);
extern __attribute__ ((visibility ("default")))
void
ucnv_setDefaultName_52 (const char *name);
extern __attribute__ ((visibility ("default")))
void
ucnv_fixFileSeparator_52 (const UConverter * cnv, UChar * source,
			  int32_t sourceLen);
extern
__attribute__ ((visibility ("default"))) UBool
ucnv_isAmbiguous_52 (const UConverter * cnv);
extern __attribute__ ((visibility ("default")))
void
ucnv_setFallback_52 (UConverter * cnv, UBool usesFallback);
extern __attribute__ ((visibility ("default"))) UBool
ucnv_usesFallback_52 (const UConverter * cnv);
extern __attribute__ ((visibility ("default")))
const char *
ucnv_detectUnicodeSignature_52 (const char *source,
				int32_t sourceLength,
				int32_t * signatureLength,
				UErrorCode * pErrorCode);
extern __attribute__ ((visibility ("default"))) int32_t
ucnv_fromUCountPending_52 (const UConverter * cnv, UErrorCode * status);
extern __attribute__ ((visibility ("default"))) int32_t
ucnv_toUCountPending_52 (const UConverter * cnv, UErrorCode * status);
extern __attribute__ ((visibility ("default"))) UBool
ucnv_isFixedWidth_52 (UConverter * cnv, UErrorCode * status);

typedef enum
  {
    XML_CHAR_ENCODING_ERROR = -1,
    XML_CHAR_ENCODING_NONE = 0,
    XML_CHAR_ENCODING_UTF8 = 1,
    XML_CHAR_ENCODING_UTF16LE = 2,
    XML_CHAR_ENCODING_UTF16BE = 3,
    XML_CHAR_ENCODING_UCS4LE = 4,
    XML_CHAR_ENCODING_UCS4BE = 5,
    XML_CHAR_ENCODING_EBCDIC = 6,
    XML_CHAR_ENCODING_UCS4_2143 = 7,
    XML_CHAR_ENCODING_UCS4_3412 = 8,
    XML_CHAR_ENCODING_UCS2 = 9,
    XML_CHAR_ENCODING_8859_1 = 10,
    XML_CHAR_ENCODING_8859_2 = 11,
    XML_CHAR_ENCODING_8859_3 = 12,
    XML_CHAR_ENCODING_8859_4 = 13,
    XML_CHAR_ENCODING_8859_5 = 14,
    XML_CHAR_ENCODING_8859_6 = 15,
    XML_CHAR_ENCODING_8859_7 = 16,
    XML_CHAR_ENCODING_8859_8 = 17,
    XML_CHAR_ENCODING_8859_9 = 18,
    XML_CHAR_ENCODING_2022_JP = 19,
    XML_CHAR_ENCODING_SHIFT_JIS = 20,
    XML_CHAR_ENCODING_EUC_JP = 21,
    XML_CHAR_ENCODING_ASCII = 22
  } xmlCharEncoding;
typedef int (*xmlCharEncodingInputFunc) (unsigned char *out, int *outlen,
					 const unsigned char *in,
					 int *inlen);
typedef int (*xmlCharEncodingOutputFunc) (unsigned char *out,
					  int *outlen,
					  const unsigned char *in,
					  int *inlen);







struct _uconv_t
{
  UConverter *
  uconv;
  UConverter *
  utf8;
};
typedef struct _uconv_t
uconv_t;


typedef struct _xmlCharEncodingHandler
xmlCharEncodingHandler;
typedef xmlCharEncodingHandler *
xmlCharEncodingHandlerPtr;
struct _xmlCharEncodingHandler
{
  char *
  name;
  xmlCharEncodingInputFunc
  input;
  xmlCharEncodingOutputFunc
  output;

  iconv_t
  iconv_in;
  iconv_t
  iconv_out;


  uconv_t *
  uconv_in;
  uconv_t *
  uconv_out;

};
void
xmlInitCharEncodingHandlers (void);
void
xmlCleanupCharEncodingHandlers (void);
void
xmlRegisterCharEncodingHandler (xmlCharEncodingHandlerPtr handler);
xmlCharEncodingHandlerPtr
xmlGetCharEncodingHandler (xmlCharEncoding enc);
xmlCharEncodingHandlerPtr
xmlFindCharEncodingHandler (const char *name);
xmlCharEncodingHandlerPtr
xmlNewCharEncodingHandler (const char *name,
			   xmlCharEncodingInputFunc input,
			   xmlCharEncodingOutputFunc output);




int xmlAddEncodingAlias (const char *name, const char *alias);
int xmlDelEncodingAlias (const char *alias);
const char *xmlGetEncodingAlias (const char *alias);
void xmlCleanupEncodingAliases (void);
xmlCharEncoding
xmlParseCharEncoding (const char *name);
const char *xmlGetCharEncodingName (xmlCharEncoding enc);




xmlCharEncoding
xmlDetectCharEncoding (const unsigned char *in, int len);

int
xmlCharEncOutFunc (xmlCharEncodingHandler * handler,
		   xmlBufferPtr out, xmlBufferPtr in);

int
xmlCharEncInFunc (xmlCharEncodingHandler * handler,
		  xmlBufferPtr out, xmlBufferPtr in);
int
xmlCharEncFirstLine (xmlCharEncodingHandler * handler,
		     xmlBufferPtr out, xmlBufferPtr in);
int xmlCharEncCloseFunc (xmlCharEncodingHandler * handler);





int
UTF8Toisolat1 (unsigned char *out,
	       int *outlen, const unsigned char *in, int *inlen);

int
isolat1ToUTF8 (unsigned char *out,
	       int *outlen, const unsigned char *in, int *inlen);
typedef int (*xmlInputMatchCallback) (char const *filename);
typedef void *(*xmlInputOpenCallback) (char const *filename);
typedef int (*xmlInputReadCallback) (void *context, char *buffer,
				     int len);
typedef int (*xmlInputCloseCallback) (void *context);
typedef int (*xmlOutputMatchCallback) (char const *filename);
typedef void *(*xmlOutputOpenCallback) (char const *filename);
typedef int (*xmlOutputWriteCallback) (void *context, const char *buffer,
				       int len);
typedef int (*xmlOutputCloseCallback) (void *context);







typedef xmlChar *xlinkHRef;
typedef xmlChar *xlinkRole;
typedef xmlChar *xlinkTitle;

typedef enum
  {
    XLINK_TYPE_NONE = 0,
    XLINK_TYPE_SIMPLE,
    XLINK_TYPE_EXTENDED,
    XLINK_TYPE_EXTENDED_SET
  } xlinkType;

typedef enum
  {
    XLINK_SHOW_NONE = 0,
    XLINK_SHOW_NEW,
    XLINK_SHOW_EMBED,
    XLINK_SHOW_REPLACE
  } xlinkShow;

typedef enum
  {
    XLINK_ACTUATE_NONE = 0,
    XLINK_ACTUATE_AUTO,
    XLINK_ACTUATE_ONREQUEST
  } xlinkActuate;
typedef void (*xlinkNodeDetectFunc) (void *ctx, xmlNodePtr node);
typedef void
(*xlinkSimpleLinkFunk) (void *ctx,
			xmlNodePtr node,
			const xlinkHRef href,
			const xlinkRole role, const xlinkTitle title);
typedef void
(*xlinkExtendedLinkFunk) (void *ctx,
			  xmlNodePtr node,
			  int nbLocators,
			  const xlinkHRef * hrefs,
			  const xlinkRole * roles,
			  int nbArcs,
			  const xlinkRole * from,
			  const xlinkRole * to,
			  xlinkShow * show,
			  xlinkActuate * actuate,
			  int nbTitles,
			  const xlinkTitle * titles,
			  const xmlChar ** langs);
typedef void
(*xlinkExtendedLinkSetFunk) (void *ctx,
			     xmlNodePtr node,
			     int nbLocators,
			     const xlinkHRef * hrefs,
			     const xlinkRole * roles,
			     int nbTitles,
			     const xlinkTitle * titles,
			     const xmlChar ** langs);







typedef struct _xlinkHandler xlinkHandler;
typedef xlinkHandler *xlinkHandlerPtr;
struct _xlinkHandler
{
  xlinkSimpleLinkFunk simple;
  xlinkExtendedLinkFunk extended;
  xlinkExtendedLinkSetFunk set;
};






xlinkNodeDetectFunc
xlinkGetDefaultDetect (void);
void xlinkSetDefaultDetect (xlinkNodeDetectFunc func);




xlinkHandlerPtr
xlinkGetDefaultHandler (void);
void xlinkSetDefaultHandler (xlinkHandlerPtr handler);




xlinkType
xlinkIsLink (xmlDocPtr doc, xmlNodePtr node);






const xmlChar *getPublicId (void *ctx);
const xmlChar *getSystemId (void *ctx);
void setDocumentLocator (void *ctx, xmlSAXLocatorPtr loc);

int getLineNumber (void *ctx);
int getColumnNumber (void *ctx);

int isStandalone (void *ctx);
int hasInternalSubset (void *ctx);
int hasExternalSubset (void *ctx);

void
internalSubset (void *ctx,
		const xmlChar * name,
		const xmlChar * ExternalID, const xmlChar * SystemID);
void
externalSubset (void *ctx,
		const xmlChar * name,
		const xmlChar * ExternalID, const xmlChar * SystemID);
xmlEntityPtr
getEntity (void *ctx, const xmlChar * name);
xmlEntityPtr
getParameterEntity (void *ctx, const xmlChar * name);
xmlParserInputPtr
resolveEntity (void *ctx, const xmlChar * publicId, const xmlChar * systemId);

void
entityDecl (void *ctx,
	    const xmlChar * name,
	    int type,
	    const xmlChar * publicId,
	    const xmlChar * systemId, xmlChar * content);
void
attributeDecl (void *ctx,
	       const xmlChar * elem,
	       const xmlChar * fullname,
	       int type,
	       int def,
	       const xmlChar * defaultValue, xmlEnumerationPtr tree);
void
elementDecl (void *ctx,
	     const xmlChar * name,
	     int type, xmlElementContentPtr content);
void
notationDecl (void *ctx,
	      const xmlChar * name,
	      const xmlChar * publicId, const xmlChar * systemId);
void
unparsedEntityDecl (void *ctx,
		    const xmlChar * name,
		    const xmlChar * publicId,
		    const xmlChar * systemId,
		    const xmlChar * notationName);

void startDocument (void *ctx);
void endDocument (void *ctx);
void
attribute (void *ctx, const xmlChar * fullname, const xmlChar * value);
void
startElement (void *ctx,
	      const xmlChar * fullname, const xmlChar ** atts);
void endElement (void *ctx, const xmlChar * name);
void reference (void *ctx, const xmlChar * name);
void characters (void *ctx, const xmlChar * ch, int len);
void ignorableWhitespace (void *ctx, const xmlChar * ch, int len);
void
processingInstruction (void *ctx,
		       const xmlChar * target, const xmlChar * data);
void
globalNamespace (void *ctx,
		 const xmlChar * href, const xmlChar * prefix);
void setNamespace (void *ctx, const xmlChar * name);
xmlNsPtr
getNamespace (void *ctx);
int checkNamespace (void *ctx, xmlChar * nameSpace);
void
namespaceDecl (void *ctx,
	       const xmlChar * href, const xmlChar * prefix);
void comment (void *ctx, const xmlChar * value);
void cdataBlock (void *ctx, const xmlChar * value, int len);


void initxmlDefaultSAXHandler (xmlSAXHandlerV1 * hdlr, int warning);

void inithtmlDefaultSAXHandler (xmlSAXHandlerV1 * hdlr);


void initdocbDefaultSAXHandler (xmlSAXHandlerV1 * hdlr);
const xmlChar *xmlSAX2GetPublicId (void *ctx);
const xmlChar *xmlSAX2GetSystemId (void *ctx);
void xmlSAX2SetDocumentLocator (void *ctx, xmlSAXLocatorPtr loc);

int xmlSAX2GetLineNumber (void *ctx);
int xmlSAX2GetColumnNumber (void *ctx);

int xmlSAX2IsStandalone (void *ctx);
int xmlSAX2HasInternalSubset (void *ctx);
int xmlSAX2HasExternalSubset (void *ctx);

void
xmlSAX2InternalSubset (void *ctx,
		       const xmlChar * name,
		       const xmlChar * ExternalID,
		       const xmlChar * SystemID);
void
xmlSAX2ExternalSubset (void *ctx,
		       const xmlChar * name,
		       const xmlChar * ExternalID,
		       const xmlChar * SystemID);
xmlEntityPtr
xmlSAX2GetEntity (void *ctx, const xmlChar * name);
xmlEntityPtr
xmlSAX2GetParameterEntity (void *ctx, const xmlChar * name);
xmlParserInputPtr
xmlSAX2ResolveEntity (void *ctx,
		      const xmlChar * publicId, const xmlChar * systemId);

void
xmlSAX2EntityDecl (void *ctx,
		   const xmlChar * name,
		   int type,
		   const xmlChar * publicId,
		   const xmlChar * systemId, xmlChar * content);
void
xmlSAX2AttributeDecl (void *ctx,
		      const xmlChar * elem,
		      const xmlChar * fullname,
		      int type,
		      int def,
		      const xmlChar * defaultValue,
		      xmlEnumerationPtr tree);
void
xmlSAX2ElementDecl (void *ctx,
		    const xmlChar * name,
		    int type, xmlElementContentPtr content);
void
xmlSAX2NotationDecl (void *ctx,
		     const xmlChar * name,
		     const xmlChar * publicId,
		     const xmlChar * systemId);
void
xmlSAX2UnparsedEntityDecl (void *ctx,
			   const xmlChar * name,
			   const xmlChar * publicId,
			   const xmlChar * systemId,
			   const xmlChar * notationName);

void xmlSAX2StartDocument (void *ctx);
void xmlSAX2EndDocument (void *ctx);



void
xmlSAX2StartElement (void *ctx,
		     const xmlChar * fullname, const xmlChar ** atts);
void xmlSAX2EndElement (void *ctx, const xmlChar * name);

void
xmlSAX2StartElementNs (void *ctx,
		       const xmlChar * localname,
		       const xmlChar * prefix,
		       const xmlChar * URI,
		       int nb_namespaces,
		       const xmlChar ** namespaces,
		       int nb_attributes,
		       int nb_defaulted, const xmlChar ** attributes);
void
xmlSAX2EndElementNs (void *ctx,
		     const xmlChar * localname,
		     const xmlChar * prefix, const xmlChar * URI);
void xmlSAX2Reference (void *ctx, const xmlChar * name);
void xmlSAX2Characters (void *ctx, const xmlChar * ch, int len);
void xmlSAX2IgnorableWhitespace (void *ctx, const xmlChar * ch, int len);
void
xmlSAX2ProcessingInstruction (void *ctx,
			      const xmlChar * target,
			      const xmlChar * data);
void xmlSAX2Comment (void *ctx, const xmlChar * value);
void xmlSAX2CDataBlock (void *ctx, const xmlChar * value, int len);


int xmlSAXDefaultVersion (int version);


int xmlSAXVersion (xmlSAXHandler * hdlr, int version);
void xmlSAX2InitDefaultSAXHandler (xmlSAXHandler * hdlr, int warning);

void xmlSAX2InitHtmlDefaultSAXHandler (xmlSAXHandler * hdlr);
void htmlDefaultSAXHandlerInit (void);


void xmlSAX2InitDocbDefaultSAXHandler (xmlSAXHandler * hdlr);
void docbDefaultSAXHandlerInit (void);

void xmlDefaultSAXHandlerInit (void);
typedef void (*xmlFreeFunc) (void *mem);
typedef void *( __attribute__ ((alloc_size (1))) *
		xmlMallocFunc) (size_t size);
typedef void *(*xmlReallocFunc) (void *mem, size_t size);
typedef char *(*xmlStrdupFunc) (const char *str);
int
xmlMemSetup (xmlFreeFunc freeFunc,
	     xmlMallocFunc mallocFunc,
	     xmlReallocFunc reallocFunc, xmlStrdupFunc strdupFunc);
int
xmlMemGet (xmlFreeFunc * freeFunc,
	   xmlMallocFunc * mallocFunc,
	   xmlReallocFunc * reallocFunc, xmlStrdupFunc * strdupFunc);
int
xmlGcMemSetup (xmlFreeFunc freeFunc,
	       xmlMallocFunc mallocFunc,
	       xmlMallocFunc mallocAtomicFunc,
	       xmlReallocFunc reallocFunc, xmlStrdupFunc strdupFunc);
int
xmlGcMemGet (xmlFreeFunc * freeFunc,
	     xmlMallocFunc * mallocFunc,
	     xmlMallocFunc * mallocAtomicFunc,
	     xmlReallocFunc * reallocFunc, xmlStrdupFunc * strdupFunc);




int xmlInitMemory (void);




void xmlCleanupMemory (void);



int xmlMemUsed (void);
int xmlMemBlocks (void);
void xmlMemDisplay (FILE * fp);
void xmlMemDisplayLast (FILE * fp, long nbBytes);
void xmlMemShow (FILE * fp, int nr);
void xmlMemoryDump (void);
void *xmlMemMalloc (size_t size) __attribute__ ((alloc_size (1)));
void *xmlMemRealloc (void *ptr, size_t size);
void xmlMemFree (void *ptr);
char *xmlMemoryStrdup (const char *str);
void *xmlMallocLoc (size_t size, const char *file, int line)
  __attribute__ ((alloc_size (1)));
void *xmlReallocLoc (void *ptr, size_t size, const char *file, int line);
void *xmlMallocAtomicLoc (size_t size, const char *file, int line)
  __attribute__ ((alloc_size (1)));
char *xmlMemStrdupLoc (const char *str, const char *file, int line);





void xmlInitGlobals (void);
void xmlCleanupGlobals (void);
typedef
xmlParserInputBufferPtr (*xmlParserInputBufferCreateFilenameFunc)
(const char *URI, xmlCharEncoding enc);
typedef xmlOutputBufferPtr (*xmlOutputBufferCreateFilenameFunc) (const
								 char
								 *URI,
								 xmlCharEncodingHandlerPtr
								 encoder,
								 int
								 compression);

xmlParserInputBufferCreateFilenameFunc
xmlParserInputBufferCreateFilenameDefault
(xmlParserInputBufferCreateFilenameFunc func);
xmlOutputBufferCreateFilenameFunc
xmlOutputBufferCreateFilenameDefault (xmlOutputBufferCreateFilenameFunc func);
typedef void (*xmlRegisterNodeFunc) (xmlNodePtr node);






typedef void (*xmlDeregisterNodeFunc) (xmlNodePtr node);

typedef struct _xmlGlobalState xmlGlobalState;
typedef xmlGlobalState *xmlGlobalStatePtr;
struct _xmlGlobalState
{
  const char *xmlParserVersion;

  xmlSAXLocator xmlDefaultSAXLocator;
  xmlSAXHandlerV1 xmlDefaultSAXHandler;
  xmlSAXHandlerV1 docbDefaultSAXHandler;
  xmlSAXHandlerV1 htmlDefaultSAXHandler;

  xmlFreeFunc xmlFree;
  xmlMallocFunc xmlMalloc;
  xmlStrdupFunc xmlMemStrdup;
  xmlReallocFunc xmlRealloc;

  xmlGenericErrorFunc xmlGenericError;
  xmlStructuredErrorFunc xmlStructuredError;
  void *xmlGenericErrorContext;

  int oldXMLWDcompatibility;

  xmlBufferAllocationScheme xmlBufferAllocScheme;
  int xmlDefaultBufferSize;

  int xmlSubstituteEntitiesDefaultValue;
  int xmlDoValidityCheckingDefaultValue;
  int xmlGetWarningsDefaultValue;
  int xmlKeepBlanksDefaultValue;
  int xmlLineNumbersDefaultValue;
  int xmlLoadExtDtdDefaultValue;
  int xmlParserDebugEntities;
  int xmlPedanticParserDefaultValue;

  int xmlSaveNoEmptyTags;
  int xmlIndentTreeOutput;
  const char *xmlTreeIndentString;

  xmlRegisterNodeFunc xmlRegisterNodeDefaultValue;
  xmlDeregisterNodeFunc xmlDeregisterNodeDefaultValue;

  xmlMallocFunc xmlMallocAtomic;
  xmlError xmlLastError;

  xmlParserInputBufferCreateFilenameFunc
  xmlParserInputBufferCreateFilenameValue;
  xmlOutputBufferCreateFilenameFunc xmlOutputBufferCreateFilenameValue;

  void *xmlStructuredErrorContext;
};




typedef struct _xmlMutex xmlMutex;
typedef xmlMutex *xmlMutexPtr;




typedef struct _xmlRMutex xmlRMutex;
typedef xmlRMutex *xmlRMutexPtr;







xmlMutexPtr
xmlNewMutex (void);
void xmlMutexLock (xmlMutexPtr tok);
void xmlMutexUnlock (xmlMutexPtr tok);
void xmlFreeMutex (xmlMutexPtr tok);

xmlRMutexPtr
xmlNewRMutex (void);
void xmlRMutexLock (xmlRMutexPtr tok);
void xmlRMutexUnlock (xmlRMutexPtr tok);
void xmlFreeRMutex (xmlRMutexPtr tok);




void xmlInitThreads (void);
void xmlLockLibrary (void);
void xmlUnlockLibrary (void);
int xmlGetThreadId (void);
int xmlIsMainThread (void);
void xmlCleanupThreads (void);
xmlGlobalStatePtr
xmlGetGlobalState (void);




void xmlInitializeGlobalState (xmlGlobalStatePtr gs);

void xmlThrDefSetGenericErrorFunc (void *ctx,
				   xmlGenericErrorFunc handler);

void xmlThrDefSetStructuredErrorFunc (void *ctx,
				      xmlStructuredErrorFunc handler);

xmlRegisterNodeFunc xmlRegisterNodeDefault (xmlRegisterNodeFunc func);
xmlRegisterNodeFunc xmlThrDefRegisterNodeDefault (xmlRegisterNodeFunc
						  func);
xmlDeregisterNodeFunc xmlDeregisterNodeDefault (xmlDeregisterNodeFunc
						func);
xmlDeregisterNodeFunc
xmlThrDefDeregisterNodeDefault (xmlDeregisterNodeFunc func);

xmlOutputBufferCreateFilenameFunc
xmlThrDefOutputBufferCreateFilenameDefault (xmlOutputBufferCreateFilenameFunc
					    func);
xmlParserInputBufferCreateFilenameFunc
xmlThrDefParserInputBufferCreateFilenameDefault
(xmlParserInputBufferCreateFilenameFunc func);
extern xmlMallocFunc xmlMalloc;
extern xmlMallocFunc xmlMallocAtomic;
extern xmlReallocFunc xmlRealloc;
extern xmlFreeFunc xmlFree;
extern xmlStrdupFunc xmlMemStrdup;



xmlSAXHandlerV1 *__docbDefaultSAXHandler (void);
xmlSAXHandlerV1 *__htmlDefaultSAXHandler (void);
xmlError *__xmlLastError (void);
int *__oldXMLWDcompatibility (void);







xmlBufferAllocationScheme *__xmlBufferAllocScheme (void);






xmlBufferAllocationScheme
xmlThrDefBufferAllocScheme (xmlBufferAllocationScheme v);

int *__xmlDefaultBufferSize (void);






int xmlThrDefDefaultBufferSize (int v);

xmlSAXHandlerV1 *__xmlDefaultSAXHandler (void);







xmlSAXLocator *__xmlDefaultSAXLocator (void);







int *__xmlDoValidityCheckingDefaultValue (void);






int xmlThrDefDoValidityCheckingDefaultValue (int v);

xmlGenericErrorFunc *__xmlGenericError (void);







xmlStructuredErrorFunc *__xmlStructuredError (void);







void **__xmlGenericErrorContext (void);







void **__xmlStructuredErrorContext (void);







int *__xmlGetWarningsDefaultValue (void);






int xmlThrDefGetWarningsDefaultValue (int v);

int *__xmlIndentTreeOutput (void);






int xmlThrDefIndentTreeOutput (int v);

const char **__xmlTreeIndentString (void);






const char *xmlThrDefTreeIndentString (const char *v);

int *__xmlKeepBlanksDefaultValue (void);






int xmlThrDefKeepBlanksDefaultValue (int v);

int *__xmlLineNumbersDefaultValue (void);






int xmlThrDefLineNumbersDefaultValue (int v);

int *__xmlLoadExtDtdDefaultValue (void);






int xmlThrDefLoadExtDtdDefaultValue (int v);

int *__xmlParserDebugEntities (void);






int xmlThrDefParserDebugEntities (int v);

const char **__xmlParserVersion (void);







int *__xmlPedanticParserDefaultValue (void);






int xmlThrDefPedanticParserDefaultValue (int v);

int *__xmlSaveNoEmptyTags (void);






int xmlThrDefSaveNoEmptyTags (int v);

int *__xmlSubstituteEntitiesDefaultValue (void);






int xmlThrDefSubstituteEntitiesDefaultValue (int v);

xmlRegisterNodeFunc *__xmlRegisterNodeDefaultValue (void);







xmlDeregisterNodeFunc *__xmlDeregisterNodeDefaultValue (void);







xmlParserInputBufferCreateFilenameFunc
*__xmlParserInputBufferCreateFilenameValue (void);







xmlOutputBufferCreateFilenameFunc
*__xmlOutputBufferCreateFilenameValue (void);







struct _xmlParserInputBuffer
{
  void *context;
  xmlInputReadCallback readcallback;
  xmlInputCloseCallback closecallback;

  xmlCharEncodingHandlerPtr encoder;

  xmlBufPtr buffer;
  xmlBufPtr raw;
  int compressed;
  int error;
  unsigned long rawconsumed;
};



struct _xmlOutputBuffer
{
  void *context;
  xmlOutputWriteCallback writecallback;
  xmlOutputCloseCallback closecallback;

  xmlCharEncodingHandlerPtr encoder;

  xmlBufPtr buffer;
  xmlBufPtr conv;
  int written;
  int error;
};





void xmlCleanupInputCallbacks (void);

int xmlPopInputCallbacks (void);

void xmlRegisterDefaultInputCallbacks (void);
xmlParserInputBufferPtr
xmlAllocParserInputBuffer (xmlCharEncoding enc);

xmlParserInputBufferPtr
xmlParserInputBufferCreateFilename (const char *URI, xmlCharEncoding enc);
xmlParserInputBufferPtr
xmlParserInputBufferCreateFile (FILE * file, xmlCharEncoding enc);
xmlParserInputBufferPtr
xmlParserInputBufferCreateFd (int fd, xmlCharEncoding enc);
xmlParserInputBufferPtr
xmlParserInputBufferCreateMem (const char *mem, int size,
			       xmlCharEncoding enc);
xmlParserInputBufferPtr
xmlParserInputBufferCreateStatic (const char *mem, int size,
				  xmlCharEncoding enc);
xmlParserInputBufferPtr
xmlParserInputBufferCreateIO (xmlInputReadCallback ioread,
			      xmlInputCloseCallback ioclose,
			      void *ioctx, xmlCharEncoding enc);
int xmlParserInputBufferRead (xmlParserInputBufferPtr in, int len);
int xmlParserInputBufferGrow (xmlParserInputBufferPtr in, int len);
int
xmlParserInputBufferPush (xmlParserInputBufferPtr in,
			  int len, const char *buf);
void xmlFreeParserInputBuffer (xmlParserInputBufferPtr in);
char *xmlParserGetDirectory (const char *filename);

int
xmlRegisterInputCallbacks (xmlInputMatchCallback matchFunc,
			   xmlInputOpenCallback openFunc,
			   xmlInputReadCallback readFunc,
			   xmlInputCloseCallback closeFunc);

xmlParserInputBufferPtr
__xmlParserInputBufferCreateFilename (const char *URI, xmlCharEncoding enc);





void xmlCleanupOutputCallbacks (void);
void xmlRegisterDefaultOutputCallbacks (void);
xmlOutputBufferPtr
xmlAllocOutputBuffer (xmlCharEncodingHandlerPtr encoder);

xmlOutputBufferPtr
xmlOutputBufferCreateFilename (const char *URI,
			       xmlCharEncodingHandlerPtr encoder,
			       int compression);

xmlOutputBufferPtr
xmlOutputBufferCreateFile (FILE * file, xmlCharEncodingHandlerPtr encoder);

xmlOutputBufferPtr
xmlOutputBufferCreateBuffer (xmlBufferPtr buffer,
			     xmlCharEncodingHandlerPtr encoder);

xmlOutputBufferPtr
xmlOutputBufferCreateFd (int fd, xmlCharEncodingHandlerPtr encoder);

xmlOutputBufferPtr
xmlOutputBufferCreateIO (xmlOutputWriteCallback iowrite,
			 xmlOutputCloseCallback ioclose,
			 void *ioctx, xmlCharEncodingHandlerPtr encoder);


const xmlChar *xmlOutputBufferGetContent (xmlOutputBufferPtr out);
size_t
xmlOutputBufferGetSize (xmlOutputBufferPtr out);

int
xmlOutputBufferWrite (xmlOutputBufferPtr out,
		      int len, const char *buf);
int xmlOutputBufferWriteString (xmlOutputBufferPtr out, const char *str);
int
xmlOutputBufferWriteEscape (xmlOutputBufferPtr out,
			    const xmlChar * str,
			    xmlCharEncodingOutputFunc escaping);

int xmlOutputBufferFlush (xmlOutputBufferPtr out);
int xmlOutputBufferClose (xmlOutputBufferPtr out);

int
xmlRegisterOutputCallbacks (xmlOutputMatchCallback matchFunc,
			    xmlOutputOpenCallback openFunc,
			    xmlOutputWriteCallback writeFunc,
			    xmlOutputCloseCallback closeFunc);

xmlOutputBufferPtr
__xmlOutputBufferCreateFilename (const char *URI,
				 xmlCharEncodingHandlerPtr encoder,
				 int compression);



void xmlRegisterHTTPPostCallbacks (void);




xmlParserInputPtr
xmlCheckHTTPInput (xmlParserCtxtPtr ctxt, xmlParserInputPtr ret);




xmlParserInputPtr
xmlNoNetExternalEntityLoader (const char *URL,
			      const char *ID, xmlParserCtxtPtr ctxt);





xmlChar *xmlNormalizeWindowsPath (const xmlChar * path);

int xmlCheckFilename (const char *path);



int xmlFileMatch (const char *filename);
void *xmlFileOpen (const char *filename);
int xmlFileRead (void *context, char *buffer, int len);
int xmlFileClose (void *context);





int xmlIOHTTPMatch (const char *filename);
void *xmlIOHTTPOpen (const char *filename);

void *xmlIOHTTPOpenW (const char *post_uri, int compression);

int xmlIOHTTPRead (void *context, char *buffer, int len);
int xmlIOHTTPClose (void *context);






int xmlIOFTPMatch (const char *filename);
void *xmlIOFTPOpen (const char *filename);
int xmlIOFTPRead (void *context, char *buffer, int len);
int xmlIOFTPClose (void *context);
void xmlInitParser (void);
void xmlCleanupParser (void);




int xmlParserInputRead (xmlParserInputPtr in, int len);
int xmlParserInputGrow (xmlParserInputPtr in, int len);





xmlDocPtr
xmlParseDoc (const xmlChar * cur);
xmlDocPtr
xmlParseFile (const char *filename);
xmlDocPtr
xmlParseMemory (const char *buffer, int size);

int xmlSubstituteEntitiesDefault (int val);
int xmlKeepBlanksDefault (int val);
void xmlStopParser (xmlParserCtxtPtr ctxt);
int xmlPedanticParserDefault (int val);
int xmlLineNumbersDefault (int val);





xmlDocPtr
xmlRecoverDoc (const xmlChar * cur);
xmlDocPtr
xmlRecoverMemory (const char *buffer, int size);
xmlDocPtr
xmlRecoverFile (const char *filename);





int xmlParseDocument (xmlParserCtxtPtr ctxt);
int xmlParseExtParsedEnt (xmlParserCtxtPtr ctxt);

int
xmlSAXUserParseFile (xmlSAXHandlerPtr sax,
		     void *user_data, const char *filename);
int
xmlSAXUserParseMemory (xmlSAXHandlerPtr sax,
		       void *user_data, const char *buffer, int size);
xmlDocPtr
xmlSAXParseDoc (xmlSAXHandlerPtr sax, const xmlChar * cur, int recovery);
xmlDocPtr
xmlSAXParseMemory (xmlSAXHandlerPtr sax,
		   const char *buffer, int size, int recovery);
xmlDocPtr
xmlSAXParseMemoryWithData (xmlSAXHandlerPtr sax,
			   const char *buffer,
			   int size, int recovery, void *data);
xmlDocPtr
xmlSAXParseFile (xmlSAXHandlerPtr sax, const char *filename, int recovery);
xmlDocPtr
xmlSAXParseFileWithData (xmlSAXHandlerPtr sax,
			 const char *filename, int recovery, void *data);
xmlDocPtr
xmlSAXParseEntity (xmlSAXHandlerPtr sax, const char *filename);
xmlDocPtr
xmlParseEntity (const char *filename);



xmlDtdPtr
xmlSAXParseDTD (xmlSAXHandlerPtr sax,
		const xmlChar * ExternalID, const xmlChar * SystemID);
xmlDtdPtr
xmlParseDTD (const xmlChar * ExternalID, const xmlChar * SystemID);
xmlDtdPtr
xmlIOParseDTD (xmlSAXHandlerPtr sax,
	       xmlParserInputBufferPtr input, xmlCharEncoding enc);


int
xmlParseBalancedChunkMemory (xmlDocPtr doc,
			     xmlSAXHandlerPtr sax,
			     void *user_data,
			     int depth,
			     const xmlChar * string, xmlNodePtr * lst);

xmlParserErrors
xmlParseInNodeContext (xmlNodePtr node,
		       const char *data,
		       int datalen, int options, xmlNodePtr * lst);

int
xmlParseBalancedChunkMemoryRecover (xmlDocPtr doc,
				    xmlSAXHandlerPtr sax,
				    void *user_data,
				    int depth,
				    const xmlChar * string,
				    xmlNodePtr * lst, int recover);
int
xmlParseExternalEntity (xmlDocPtr doc,
			xmlSAXHandlerPtr sax,
			void *user_data,
			int depth,
			const xmlChar * URL,
			const xmlChar * ID, xmlNodePtr * lst);

int
xmlParseCtxtExternalEntity (xmlParserCtxtPtr ctx,
			    const xmlChar * URL,
			    const xmlChar * ID, xmlNodePtr * lst);




xmlParserCtxtPtr
xmlNewParserCtxt (void);
int xmlInitParserCtxt (xmlParserCtxtPtr ctxt);
void xmlClearParserCtxt (xmlParserCtxtPtr ctxt);
void xmlFreeParserCtxt (xmlParserCtxtPtr ctxt);

void
xmlSetupParserForBuffer (xmlParserCtxtPtr ctxt,
			 const xmlChar * buffer, const char *filename);

xmlParserCtxtPtr
xmlCreateDocParserCtxt (const xmlChar * cur);





int xmlGetFeaturesList (int *len, const char **result);
int
xmlGetFeature (xmlParserCtxtPtr ctxt, const char *name, void *result);
int xmlSetFeature (xmlParserCtxtPtr ctxt, const char *name, void *value);






xmlParserCtxtPtr
xmlCreatePushParserCtxt (xmlSAXHandlerPtr sax,
			 void *user_data,
			 const char *chunk, int size, const char *filename);
int
xmlParseChunk (xmlParserCtxtPtr ctxt,
	       const char *chunk, int size, int terminate);






xmlParserCtxtPtr
xmlCreateIOParserCtxt (xmlSAXHandlerPtr sax,
		       void *user_data,
		       xmlInputReadCallback ioread,
		       xmlInputCloseCallback ioclose,
		       void *ioctx, xmlCharEncoding enc);

xmlParserInputPtr
xmlNewIOInputStream (xmlParserCtxtPtr ctxt,
		     xmlParserInputBufferPtr input, xmlCharEncoding enc);




const xmlParserNodeInfo *xmlParserFindNodeInfo (const xmlParserCtxtPtr
						ctxt,
						const xmlNodePtr node);
void xmlInitNodeInfoSeq (xmlParserNodeInfoSeqPtr seq);
void xmlClearNodeInfoSeq (xmlParserNodeInfoSeqPtr seq);
unsigned long
xmlParserFindNodeInfoIndex (const xmlParserNodeInfoSeqPtr seq,
			    const xmlNodePtr node);
void
xmlParserAddNodeInfo (xmlParserCtxtPtr ctxt,
		      const xmlParserNodeInfoPtr info);





void xmlSetExternalEntityLoader (xmlExternalEntityLoader f);
xmlExternalEntityLoader
xmlGetExternalEntityLoader (void);
xmlParserInputPtr
xmlLoadExternalEntity (const char *URL,
		       const char *ID, xmlParserCtxtPtr ctxt);




long xmlByteConsumed (xmlParserCtxtPtr ctxt);
typedef enum
  {
    XML_PARSE_RECOVER = 1 << 0,
    XML_PARSE_NOENT = 1 << 1,
    XML_PARSE_DTDLOAD = 1 << 2,
    XML_PARSE_DTDATTR = 1 << 3,
    XML_PARSE_DTDVALID = 1 << 4,
    XML_PARSE_NOERROR = 1 << 5,
    XML_PARSE_NOWARNING = 1 << 6,
    XML_PARSE_PEDANTIC = 1 << 7,
    XML_PARSE_NOBLANKS = 1 << 8,
    XML_PARSE_SAX1 = 1 << 9,
    XML_PARSE_XINCLUDE = 1 << 10,
    XML_PARSE_NONET = 1 << 11,
    XML_PARSE_NODICT = 1 << 12,
    XML_PARSE_NSCLEAN = 1 << 13,
    XML_PARSE_NOCDATA = 1 << 14,
    XML_PARSE_NOXINCNODE = 1 << 15,
    XML_PARSE_COMPACT = 1 << 16,


    XML_PARSE_OLD10 = 1 << 17,
    XML_PARSE_NOBASEFIX = 1 << 18,
    XML_PARSE_HUGE = 1 << 19,
    XML_PARSE_OLDSAX = 1 << 20,
    XML_PARSE_IGNORE_ENC = 1 << 21,
    XML_PARSE_BIG_LINES = 1 << 22
  } xmlParserOption;

void xmlCtxtReset (xmlParserCtxtPtr ctxt);
int
xmlCtxtResetPush (xmlParserCtxtPtr ctxt,
		  const char *chunk,
		  int size,
		  const char *filename, const char *encoding);
int xmlCtxtUseOptions (xmlParserCtxtPtr ctxt, int options);
xmlDocPtr
xmlReadDoc (const xmlChar * cur,
	    const char *URL, const char *encoding, int options);
xmlDocPtr
xmlReadFile (const char *URL, const char *encoding, int options);
xmlDocPtr
xmlReadMemory (const char *buffer,
	       int size, const char *URL, const char *encoding, int options);
xmlDocPtr
xmlReadFd (int fd, const char *URL, const char *encoding, int options);
xmlDocPtr
xmlReadIO (xmlInputReadCallback ioread,
	   xmlInputCloseCallback ioclose,
	   void *ioctx, const char *URL, const char *encoding, int options);
xmlDocPtr
xmlCtxtReadDoc (xmlParserCtxtPtr ctxt,
		const xmlChar * cur,
		const char *URL, const char *encoding, int options);
xmlDocPtr
xmlCtxtReadFile (xmlParserCtxtPtr ctxt,
		 const char *filename, const char *encoding, int options);
xmlDocPtr
xmlCtxtReadMemory (xmlParserCtxtPtr ctxt,
		   const char *buffer,
		   int size,
		   const char *URL, const char *encoding, int options);
xmlDocPtr
xmlCtxtReadFd (xmlParserCtxtPtr ctxt,
	       int fd, const char *URL, const char *encoding, int options);
xmlDocPtr
xmlCtxtReadIO (xmlParserCtxtPtr ctxt,
	       xmlInputReadCallback ioread,
	       xmlInputCloseCallback ioclose,
	       void *ioctx,
	       const char *URL, const char *encoding, int options);
typedef enum
  {
    XML_WITH_THREAD = 1,
    XML_WITH_TREE = 2,
    XML_WITH_OUTPUT = 3,
    XML_WITH_PUSH = 4,
    XML_WITH_READER = 5,
    XML_WITH_PATTERN = 6,
    XML_WITH_WRITER = 7,
    XML_WITH_SAX1 = 8,
    XML_WITH_FTP = 9,
    XML_WITH_HTTP = 10,
    XML_WITH_VALID = 11,
    XML_WITH_HTML = 12,
    XML_WITH_LEGACY = 13,
    XML_WITH_C14N = 14,
    XML_WITH_CATALOG = 15,
    XML_WITH_XPATH = 16,
    XML_WITH_XPTR = 17,
    XML_WITH_XINCLUDE = 18,
    XML_WITH_ICONV = 19,
    XML_WITH_ISO8859X = 20,
    XML_WITH_UNICODE = 21,
    XML_WITH_REGEXP = 22,
    XML_WITH_AUTOMATA = 23,
    XML_WITH_EXPR = 24,
    XML_WITH_SCHEMAS = 25,
    XML_WITH_SCHEMATRON = 26,
    XML_WITH_MODULES = 27,
    XML_WITH_DEBUG = 28,
    XML_WITH_DEBUG_MEM = 29,
    XML_WITH_DEBUG_RUN = 30,
    XML_WITH_ZLIB = 31,
    XML_WITH_ICU = 32,
    XML_WITH_LZMA = 33,
    XML_WITH_NONE = 99999
  } xmlFeature;

int xmlHasFeature (xmlFeature feature);
typedef unsigned long int ub4;



typedef signed long int sb4;



typedef unsigned short int ub2;



typedef signed short int sb2;



typedef unsigned char ub1;



typedef signed char sb1;



typedef int word;
struct randctx
{
  ub4 randcnt;
  ub4 randrsl[(1 << (4))];
  ub4 randmem[(1 << (4))];
  ub4 randa;
  ub4 randb;
  ub4 randc;
};
typedef struct randctx randctx;






void irandinit (randctx * r, word flag);

void isaac (randctx * r);
char *flam3_version ();
typedef struct
{
  double index;
  double color[4];
} flam3_palette_entry;

typedef flam3_palette_entry flam3_palette[256];

int flam3_get_palette (int palette_index, flam3_palette p,
		       double hue_rotation);




extern char *flam3_variation_names[];
typedef struct
{

  double badvals;
  long int num_iters;
  int render_seconds;

} stat_struct;

typedef struct
{

  unsigned int width, height;
  int version;
  int id;


  double intensity_weight[256];
  unsigned int bin_size[256];
  unsigned int bin_offset[256];





  unsigned short *rowcols;

} flam3_image_store;


typedef struct xform
{
  double var[98];
  double c[3][2];
  double post[3][2];
  double density;
  double color;
  double color_speed;
  double animate;
  double opacity;
  double vis_adjusted;

  int padding;
  double wind[2];

  int precalc_angles_flag;
  int precalc_atan_xy_flag;
  int precalc_atan_yx_flag;
  double has_preblur;
  int has_post;



  double blob_low;
  double blob_high;
  double blob_waves;


  double pdj_a;
  double pdj_b;
  double pdj_c;
  double pdj_d;


  double fan2_x;
  double fan2_y;


  double rings2_val;


  double perspective_angle;
  double perspective_dist;


  double julian_power;
  double julian_dist;


  double juliascope_power;
  double juliascope_dist;


  double radial_blur_angle;


  double pie_slices;
  double pie_rotation;
  double pie_thickness;


  double ngon_sides;
  double ngon_power;
  double ngon_circle;
  double ngon_corners;


  double curl_c1;
  double curl_c2;


  double rectangles_x;
  double rectangles_y;


  double amw_amp;


  double disc2_rot;
  double disc2_twist;


  double super_shape_rnd;
  double super_shape_m;
  double super_shape_n1;
  double super_shape_n2;
  double super_shape_n3;
  double super_shape_holes;


  double flower_petals;
  double flower_holes;


  double conic_eccentricity;
  double conic_holes;


  double parabola_height;
  double parabola_width;


  double bent2_x;
  double bent2_y;


  double bipolar_shift;


  double cell_size;


  double cpow_r;
  double cpow_i;
  double cpow_power;


  double curve_xamp, curve_yamp;
  double curve_xlength, curve_ylength;


  double escher_beta;


  double lazysusan_spin;
  double lazysusan_space;
  double lazysusan_twist;
  double lazysusan_x, lazysusan_y;


  double modulus_x, modulus_y;


  double oscope_separation;
  double oscope_frequency;
  double oscope_amplitude;
  double oscope_damping;


  double popcorn2_x, popcorn2_y, popcorn2_c;


  double separation_x, separation_xinside;
  double separation_y, separation_yinside;


  double split_xsize;
  double split_ysize;


  double splits_x, splits_y;


  double stripes_space;
  double stripes_warp;


  double wedge_angle, wedge_hole;
  double wedge_count, wedge_swirl;


  double wedge_julia_angle;
  double wedge_julia_count;
  double wedge_julia_power;
  double wedge_julia_dist;


  double wedge_sph_angle, wedge_sph_count;
  double wedge_sph_hole, wedge_sph_swirl;


  double whorl_inside, whorl_outside;


  double waves2_freqx, waves2_scalex;
  double waves2_freqy, waves2_scaley;


  double auger_sym, auger_weight;
  double auger_freq, auger_scale;


  double flux_spread;



  double persp_vsin;
  double persp_vfcos;


  double julian_rN;
  double julian_cn;


  double juliascope_rN;
  double juliascope_cn;


  double wedgeJulia_rN;
  double wedgeJulia_cn;
  double wedgeJulia_cf;


  double radialBlur_spinvar;
  double radialBlur_zoomvar;


  double waves_dx2;
  double waves_dy2;


  double disc2_sinadd;
  double disc2_cosadd;
  double disc2_timespi;


  double super_shape_pm_4;
  double super_shape_pneg1_n1;

  int num_active_vars;
  double active_var_weights[98];
  int varFunc[98];

  int motion_freq;
  int motion_func;

  struct xform *motion;
  int num_motion;


} flam3_xform;

typedef struct
{
  char flame_name[64 + 1];
  double time;
  int interpolation;
  int interpolation_type;
  int palette_interpolation;
  int num_xforms;
  int final_xform_index;
  int final_xform_enable;
  flam3_xform *xform;


  double **chaos;
  int chaos_enable;

  int genome_index;
  char parent_fname[30];
  int symmetry;
  flam3_palette palette;
  char *input_image;
  int palette_index;
  double brightness;
  double contrast;
  double gamma;
  double highlight_power;
  int width, height;
  int spatial_oversample;
  double center[2];
  double rot_center[2];
  double rotate;
  double vibrancy;
  double hue_rotation;
  double background[3];
  double zoom;
  double pixels_per_unit;
  double spatial_filter_radius;
  int spatial_filter_select;


  double sample_density;






  int nbatches;
  int ntemporal_samples;


  double estimator;
  double estimator_curve;
  double estimator_minimum;



  xmlDocPtr edits;


  double gam_lin_thresh;


  int palette_index0;
  double hue_rotation0;
  int palette_index1;
  double hue_rotation1;
  double palette_blend;

  int temporal_filter_type;
  double temporal_filter_width, temporal_filter_exp;

  int palette_mode;


} flam3_genome;

typedef struct
{
  int from;
  int to;
  double scalar;
} flam3_chaos_entry;



void flam3_add_motion_element (flam3_xform * xf);
void flam3_add_xforms (flam3_genome * cp, int num_to_add,
		       int interp_padding, int final_flag);
void flam3_delete_xform (flam3_genome * thiscp, int idx_to_delete);
void flam3_copy_xform (flam3_xform * dest, flam3_xform * src);
void flam3_copy (flam3_genome * dest, flam3_genome * src);
void flam3_copyx (flam3_genome * dest, flam3_genome * src, int num_std,
		  int num_final);
void flam3_copy_params (flam3_xform * dest, flam3_xform * src, int varn);
void flam3_delete_motion_elements (flam3_xform * xf);

int flam3_xform_preview (flam3_genome * cp, int xi, double range,
			 int numvals, int depth, double *result,
			 randctx * rc);
unsigned short *flam3_create_xform_distrib (flam3_genome * cp);
int flam3_create_chaos_distrib (flam3_genome * cp, int xi,
				unsigned short *xform_distrib);
int flam3_check_unity_chaos (flam3_genome * cp);
void clear_cp (flam3_genome * cp, int def_flag);






int flam3_iterate (flam3_genome * g, int nsamples, int fuse,
		   double *samples, unsigned short *xform_distrib,
		   randctx * rc);

void apply_motion_parameters (flam3_xform * xf, flam3_xform * addto,
			      double blend);



void flam3_interpolate (flam3_genome * genomes, int ngenomes,
			double time, double stagger,
			flam3_genome * result);


void flam3_print (FILE * f, flam3_genome * g, char *extra_attributes,
		  int print_edits);
void flam3_print_xform (FILE * f, flam3_xform * x, int final_flag,
			int numstd, double *chaos_row, int motion_flag);
char *flam3_print_to_string (flam3_genome * cp);





void flam3_random (flam3_genome * g, int *ivars, int ivars_n, int sym,
		   int spec_xforms);

void add_to_action (char *action, char *addtoaction);

void flam3_mutate (flam3_genome * cp, int mutate_mode, int *ivars,
		   int ivars_n, int sym, double speed, randctx * rc,
		   char *action);
void flam3_cross (flam3_genome * cp0, flam3_genome * cp1,
		  flam3_genome * out, int cross_mode, randctx * rc,
		  char *action);


flam3_genome *flam3_parse_xml2 (char *s, char *fn, int default_flag,
				int *ncps);
flam3_genome *flam3_parse_from_file (FILE * f, char *fn,
				     int default_flag, int *ncps);

void flam3_add_symmetry (flam3_genome * g, int sym);

void flam3_improve_colors (flam3_genome * g, int ntries,
			   int change_palette, int color_resolution);
int flam3_colorhist (flam3_genome * cp, int num_batches, randctx * rc,
		     double *hist);
int flam3_estimate_bounding_box (flam3_genome * g, double eps,
				 int nsamples, double *bmin,
				 double *bmax, randctx * rc);
void flam3_rotate (flam3_genome * g, double angle, int interp_type);

double flam3_dimension (flam3_genome * g, int ntries,
			int clip_to_camera);
double flam3_lyapunov (flam3_genome * g, int ntries);

void flam3_apply_template (flam3_genome * cp, flam3_genome * templ);

int flam3_count_nthreads (void);

typedef struct
{

  double pixel_aspect_ratio;
  flam3_genome *genomes;
  int ngenomes;
  int verbose;
  int bits;
  int bytes_per_channel;
  int earlyclip;
  double time;
  int (*progress) (void *, double, int, double);
  void *progress_parameter;
  randctx rc;
  int nthreads;
  int sub_batch_size;
} flam3_frame;
int flam3_render (flam3_frame * f, void *out, int field, int nchan,
		  int transp, stat_struct * stats);

double flam3_render_memory_required (flam3_frame * f);
int flam3_make_strip (flam3_genome * cp, int nstrips, int stripnum);
void rotate_by (double *p, double *center, double by);


double flam3_random01 ();
double flam3_random11 ();
int flam3_random_bit ();


double flam3_random_isaac_01 (randctx *);
double flam3_random_isaac_11 (randctx *);
int flam3_random_isaac_bit (randctx *);

void flam3_init_frame (flam3_frame * f);


void *flam3_malloc (size_t size);
void flam3_free (void *ptr);

void flam3_srandom ();

flam3_genome *sheep_loop (flam3_genome * cp, double blend);
flam3_genome *sheep_edge (flam3_genome * cp, double blend, int seqflag,
			  double stagger);





extern char *dirname (char *__path)
  __attribute__ ((__nothrow__, __leaf__));







extern char *__xpg_basename (char *__path)
  __attribute__ ((__nothrow__, __leaf__));






void docstring ();


typedef struct
{
  unsigned short *xform_distrib;
  flam3_frame *spec;
  double bounds[4];
  double rot[2][2];
  double size[2];
  int width, height;
  double ws0, wb0s0, hs1, hb1s1;
  flam3_palette_entry *dmap;
  double color_scalar;
  void *buckets;
  double badvals;
  double batch_size;
  int temporal_sample_num, ntemporal_samples;
  int batch_num, nbatches, aborted;
  time_t *progress_timer;
  time_t *progress_timer_history;
  double *progress_history;
  int *progress_history_mark;


  pthread_mutex_t bucket_mutex;


} flam3_iter_constants;

typedef struct
{

  double tx, ty;

  double precalc_atan, precalc_sina;
  double precalc_cosa, precalc_sqrt;
  double precalc_sumsq, precalc_atanyx;

  flam3_xform *xform;



  double p0, p1;


  randctx *rc;

} flam3_iter_helper;

typedef struct
{
  double *iter_storage;
  randctx rc;
  flam3_genome cp;
  int first_thread;
  int timer_initialize;
  flam3_iter_constants *fic;
} flam3_thread_helper;

double flam3_sinc (double x);


double flam3_gaussian_filter (double x);
double flam3_hermite_filter (double t);
double flam3_box_filter (double t);
double flam3_triangle_filter (double t);
double flam3_bell_filter (double t);
double flam3_b_spline_filter (double t);
double flam3_lanczos3_filter (double t);
double flam3_lanczos2_filter (double t);
double flam3_mitchell_filter (double t);
double flam3_blackman_filter (double x);
double flam3_catrom_filter (double x);
double flam3_hamming_filter (double x);
double flam3_hanning_filter (double x);
double flam3_quadratic_filter (double x);

double flam3_spatial_filter (int knum, double x);
typedef struct
{
  int max_filtered_counts;
  int max_filter_index;
  int kernel_size;
  double *filter_widths;
  double *filter_coefs;
} flam3_de_helper;

extern double flam3_spatial_support[14];

double flam3_spatial_filter (int knum, double x);
int flam3_create_spatial_filter (flam3_frame * spec, int field,
				 double **filter);
flam3_de_helper flam3_create_de_filters (double max_rad, double min_rad,
					 double curve, int ss);
double flam3_create_temporal_filter (int numsteps, int filter_type,
				     double filter_exp,
				     double filter_width,
				     double **temporal_filter,
				     double **temporal_deltas);

double flam3_spatial_support[14] = {
  1.5,
  1.0,
  0.5,
  1.0,
  1.5,
  2.0,
  2.0,
  1.0,
  2.0,
  1.0,
  1.0,
  3.0,
  2.0,
  1.5
};

double
flam3_hermite_filter (double t)
{

  if (t < 0.0)
    t = -t;
  if (t < 1.0)
    return ((2.0 * t - 3.0) * t * t + 1.0);
  return (0.0);
}

double
flam3_box_filter (double t)
{
  if ((t > -0.5) && (t <= 0.5))
    return (1.0);
  return (0.0);
}

double
flam3_triangle_filter (double t)
{
  if (t < 0.0)
    t = -t;
  if (t < 1.0)
    return (1.0 - t);
  return (0.0);
}

double
flam3_bell_filter (double t)
{

  if (t < 0)
    t = -t;
  if (t < .5)
    return (.75 - (t * t));
  if (t < 1.5)
    {
      t = (t - 1.5);
      return (.5 * (t * t));
    }
  return (0.0);
}

double
flam3_b_spline_filter (double t)
{


  double tt;

  if (t < 0)
    t = -t;
  if (t < 1)
    {
      tt = t * t;
      return ((.5 * tt * t) - tt + (2.0 / 3.0));
    }
  else if (t < 2)
    {
      t = 2 - t;
      return ((1.0 / 6.0) * (t * t * t));
    }
  return (0.0);
}

double
flam3_sinc (double x)
{
  x *= 3.14159265358979323846;
  if (x != 0)
    return (sin (x) / x);
  return (1.0);
}

double
flam3_blackman_filter (double x)
{
  return (0.42 + 0.5 * cos (3.14159265358979323846 * x) + 0.08 * cos (2 * 3.14159265358979323846 * x));
}

double
flam3_catrom_filter (double x)
{
  if (x < -2.0)
    return (0.0);
  if (x < -1.0)
    return (0.5 * (4.0 + x * (8.0 + x * (5.0 + x))));
  if (x < 0.0)
    return (0.5 * (2.0 + x * x * (-5.0 - 3.0 * x)));
  if (x < 1.0)
    return (0.5 * (2.0 + x * x * (-5.0 + 3.0 * x)));
  if (x < 2.0)
    return (0.5 * (4.0 + x * (-8.0 + x * (5.0 - x))));
  return (0.0);
}

double
flam3_mitchell_filter (double t)
{
  double tt;

  tt = t * t;
  if (t < 0)
    t = -t;
  if (t < 1.0)
    {
      t = (((12.0 - 9.0 * (1.0 / 3.0) - 6.0 * (1.0 / 3.0)) * (t * tt))
	   + ((-18.0 + 12.0 * (1.0 / 3.0) + 6.0 * (1.0 / 3.0)) * tt)
	   + (6.0 - 2 * (1.0 / 3.0)));
      return (t / 6.0);
    }
  else if (t < 2.0)
    {
      t = (((-1.0 * (1.0 / 3.0) - 6.0 * (1.0 / 3.0)) * (t * tt))
	   + ((6.0 * (1.0 / 3.0) + 30.0 * (1.0 / 3.0)) * tt)
	   + ((-12.0 * (1.0 / 3.0) - 48.0 * (1.0 / 3.0)) * t)
	   + (8.0 * (1.0 / 3.0) + 24 * (1.0 / 3.0)));
      return (t / 6.0);
    }
  return (0.0);
}

double
flam3_hanning_filter (double x)
{
  return (0.5 + 0.5 * cos (3.14159265358979323846 * x));
}

double
flam3_hamming_filter (double x)
{
  return (0.54 + 0.46 * cos (3.14159265358979323846 * x));
}

double
flam3_lanczos3_filter (double t)
{
  if (t < 0)
    t = -t;
  if (t < 3.0)
    return (flam3_sinc (t) * flam3_sinc (t / 3.0));
  return (0.0);
}

double
flam3_lanczos2_filter (double t)
{
  if (t < 0)
    t = -t;
  if (t < 2.0)
    return (flam3_sinc (t) * flam3_sinc (t / 2.0));
  return (0.0);
}

double
flam3_gaussian_filter (double x)
{
  return (exp ((-2.0 * x * x)) * sqrt (2.0 / 3.14159265358979323846));
}

double
flam3_quadratic_filter (double x)
{
  if (x < -1.5)
    return (0.0);
  if (x < -0.5)
    return (0.5 * (x + 1.5) * (x + 1.5));
  if (x < 0.5)
    return (0.75 - x * x);
  if (x < 1.5)
    return (0.5 * (x - 1.5) * (x - 1.5));
  return (0.0);
}

double
flam3_spatial_filter (int knum, double x)
{

  if (knum == 0)
    return flam3_gaussian_filter (x);
  else if (knum == 1)
    return flam3_hermite_filter (x);
  else if (knum == 2)
    return flam3_box_filter (x);
  else if (knum == 3)
    return flam3_triangle_filter (x);
  else if (knum == 4)
    return flam3_bell_filter (x);
  else if (knum == 5)
    return flam3_b_spline_filter (x);
  else if (knum == 6)
    return flam3_mitchell_filter (x);
  else if (knum == 7)
    return flam3_sinc (x) * flam3_blackman_filter (x);
  else if (knum == 8)
    return flam3_catrom_filter (x);
  else if (knum == 9)
    return flam3_sinc (x) * flam3_hanning_filter (x);
  else if (knum == 10)
    return flam3_sinc (x) * flam3_hamming_filter (x);
  else if (knum == 11)
    return flam3_lanczos3_filter (x) * flam3_sinc (x / 3.0);
  else if (knum == 12)
    return flam3_lanczos2_filter (x) * flam3_sinc (x / 2.0);
  else if (knum == 13)
    return flam3_quadratic_filter (x);
}

int
normalize_vector (double *v, int n)
{
  double t = 0.0;
  int i;
  for (i = 0; i < n; i++)
    t += v[i];
  if (0.0 == t)
    return 1;
  t = 1.0 / t;
  for (i = 0; i < n; i++)
    v[i] *= t;
  return 0;
}


int
flam3_create_spatial_filter (flam3_frame * spec, int field, double **filter)
{

  int sf_kernel = spec->genomes[0].spatial_filter_select;
  int supersample = spec->genomes[0].spatial_oversample;
  double sf_radius = spec->genomes[0].spatial_filter_radius;
  double aspect_ratio = spec->pixel_aspect_ratio;
  double sf_supp = flam3_spatial_support[sf_kernel];

  double fw = 2.0 * sf_supp * supersample * sf_radius / aspect_ratio;
  double adjust, ii, jj;

  int fwidth = ((int) fw) + 1;
  int i, j;



  if ((fwidth ^ supersample) & 1)
    fwidth++;


  if (fw > 0.0)
    adjust = sf_supp * fwidth / fw;
  else
    adjust = 1.0;


  (*filter) = (double *) calloc (fwidth * fwidth, sizeof (double));


  for (i = 0; i < fwidth; i++)
    for (j = 0; j < fwidth; j++)
      {


	ii = ((2.0 * i + 1.0) / (double) fwidth - 1.0) * adjust;
	jj = ((2.0 * j + 1.0) / (double) fwidth - 1.0) * adjust;


	if (field)
	  jj *= 2.0;


	jj /= aspect_ratio;

	(*filter)[i + j * fwidth] =
	  flam3_spatial_filter (sf_kernel,
				ii) * flam3_spatial_filter (sf_kernel, jj);
      }


  if (normalize_vector ((*filter), fwidth * fwidth))
    {
      fprintf (stderr,
	       "Spatial filter value is too small: %g.  Terminating.\n",
	       sf_radius);
      return (-1);
    }

  return (fwidth);
}

flam3_de_helper
flam3_create_de_filters (double max_rad, double min_rad, double curve, int ss)
{

  flam3_de_helper de;
  double comp_max_radius, comp_min_radius;
  double num_de_filters_d;
  int num_de_filters, de_max_ind;
  int de_row_size, de_half_size;
  int filtloop;
  int keep_thresh = 100;

  de.kernel_size = -1;

  if (curve <= 0.0)
    {
      fprintf (stderr, "estimator curve must be > 0\n");
      return (de);
    }

  if (max_rad < min_rad)
    {
      fprintf (stderr, "estimator must be larger than estimator_minimum.\n");
      fprintf (stderr, "(%f > %f) ? \n", max_rad, min_rad);
      return (de);
    }



  comp_max_radius = max_rad * ss + 1;
  comp_min_radius = min_rad * ss + 1;





  num_de_filters_d = pow (comp_max_radius / comp_min_radius, 1.0 / curve);
  if (num_de_filters_d > 1e7)
    {
      fprintf (stderr,
	       "too many filters required in this configuration (%g)\n",
	       num_de_filters_d);
      return (de);
    }
  num_de_filters = (int) ceil (num_de_filters_d);


  if (num_de_filters > keep_thresh)
    {
      de_max_ind = (int) ceil (100 + pow (num_de_filters - 100, curve)) + 1;
      de.max_filtered_counts =
	(int) pow ((double) (de_max_ind - 100), 1.0 / curve) + 100;
    }
  else
    {
      de_max_ind = num_de_filters;
      de.max_filtered_counts = de_max_ind;
    }



  de_row_size = (int) (2 * ceil (comp_max_radius) - 1);
  de_half_size = (de_row_size - 1) / 2;
  de.kernel_size = (de_half_size + 1) * (2 + de_half_size) / 2;

  de.filter_coefs =
    (double *) calloc (de_max_ind * de.kernel_size, sizeof (double));
  de.filter_widths = (double *) calloc (de_max_ind, sizeof (double));


  de.max_filter_index = 0;
  for (filtloop = 0; filtloop < de_max_ind; filtloop++)
    {

      double de_filt_sum = 0.0, de_filt_d;
      double de_filt_h;
      int dej, dek;
      double adjloop;
      int filter_coef_idx;


      if (filtloop < keep_thresh)
	de_filt_h = (comp_max_radius / pow (filtloop + 1, curve));
      else
	{
	  adjloop = pow (filtloop - keep_thresh, (1.0 / curve)) + keep_thresh;
	  de_filt_h = (comp_max_radius / pow (adjloop + 1, curve));
	}


      if (de_filt_h <= comp_min_radius)
	{
	  de_filt_h = comp_min_radius;
	  de.max_filter_index = filtloop;
	}

      de.filter_widths[filtloop] = de_filt_h;


      for (dej = -de_half_size; dej <= de_half_size; dej++)
	{
	  for (dek = -de_half_size; dek <= de_half_size; dek++)
	    {

	      de_filt_d = sqrt ((double) (dej * dej + dek * dek)) / de_filt_h;


	      if (de_filt_d <= 1.0)
		{
		  de_filt_sum += flam3_spatial_filter (0, flam3_spatial_support [0] * de_filt_d);
		}
	    }
	}

      filter_coef_idx = filtloop * de.kernel_size;


      for (dej = 0; dej <= de_half_size; dej++)
	{
	  for (dek = 0; dek <= dej; dek++)
	    {
	      de_filt_d = sqrt ((double) (dej * dej + dek * dek)) / de_filt_h;


	      if (de_filt_d > 1.0)
		de.filter_coefs[filter_coef_idx] = 0.0;
	      else
		{
		  de.filter_coefs[filter_coef_idx] = flam3_spatial_filter (0, flam3_spatial_support[0] * de_filt_d) / de_filt_sum;
		}

	      filter_coef_idx++;
	    }
	}

      if (de.max_filter_index > 0)
	break;
    }

  if (de.max_filter_index == 0)
    de.max_filter_index = de_max_ind - 1;


  return (de);
}

double
flam3_create_temporal_filter (int numsteps, int filter_type,
			      double filter_exp, double filter_width,
			      double **temporal_filter,
			      double **temporal_deltas)
{

  double maxfilt = 0.0;
  double sumfilt = 0.0;
  double slpx, halfsteps;
  double *deltas, *filter;

  int i;


  deltas = (double *) malloc (numsteps * sizeof (double));
  filter = (double *) malloc (numsteps * sizeof (double));


  if (numsteps == 1)
    {
      deltas[0] = 0;
      filter[0] = 1.0;
      *temporal_deltas = deltas;
      *temporal_filter = filter;
      return (1.0);
    }


  for (i = 0; i < numsteps; i++)
    deltas[i] = ((double) i / (double) (numsteps - 1) - 0.5) * filter_width;


  if (2 == filter_type)
    {

      for (i = 0; i < numsteps; i++)
	{

	  if (filter_exp >= 0)
	    slpx = ((double) i + 1.0) / numsteps;
	  else
	    slpx = (double) (numsteps - i) / numsteps;


	  filter[i] = pow (slpx, fabs (filter_exp));


	  if (filter[i] > maxfilt)
	    maxfilt = filter[i];
	}

    }
  else if (1 == filter_type)
    {

      halfsteps = numsteps / 2.0;
      for (i = 0; i < numsteps; i++)
	{


	  filter[i] = flam3_spatial_filter (0, flam3_spatial_support[0] * fabs (i - halfsteps) / halfsteps);
	  if (filter[i] > maxfilt)
	    maxfilt = filter[i];
	}

    }
  else
    {

      for (i = 0; i < numsteps; i++)
	filter[i] = 1.0;

      maxfilt = 1.0;

    }



  for (i = 0; i < numsteps; i++)
    {
      filter[i] /= maxfilt;
      sumfilt += filter[i];
    }

  sumfilt /= numsteps;

  *temporal_deltas = deltas;
  *temporal_filter = filter;

  return (sumfilt);
}
