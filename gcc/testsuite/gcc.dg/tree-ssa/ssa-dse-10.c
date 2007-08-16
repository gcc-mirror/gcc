/* { dg-do compile } */
/* { dg-options "-O -w -fdump-tree-dse-vops" } */

typedef unsigned int size_t;
typedef struct _IO_FILE FILE;
typedef struct
{
} __mbstate_t;
typedef struct
{
} _G_fpos_t;
typedef struct
{
};
typedef int (*__gconv_trans_fct) (struct __gconv_step *,
      size_t *);
typedef int (*__gconv_trans_context_fct) (void *, __const unsigned char *,
     size_t *);
struct __gconv_trans_data
{
};
struct __gconv_step_data
{
};
typedef struct __gconv_info
{
} *__gconv_t;
typedef union
{
  struct
  {
  } __combined;
} _G_iconv_t;
typedef __builtin_va_list __gnuc_va_list;
enum __codecvt_result
{
  __codecvt_noconv
};
struct _IO_FILE {
};
vprintf (__const char *__restrict __fmt, __gnuc_va_list __arg)
{
}
putchar (int __c)
{
}
fputc_unlocked (int __c, FILE *__stream)
{
}
putc_unlocked (int __c, FILE *__stream)
{
}
__attribute__ ((__nothrow__)) ferror_unlocked (FILE *__stream)
{
}
extern int __sprintf_chk (char *__restrict __s, int __flag, size_t __slen,
     __gnuc_va_list __ap);
gets (char *__str)
{
}
extern char *__fgets_chk (char *__restrict __s, size_t __size, int __n,
     FILE *__restrict __stream) __attribute__ ((__warn_unused_result__));
fgets (char *__restrict __s, int __n, FILE *__restrict __stream)
{
}
typedef void *LPVOID;
typedef int BOOL, *PBOOL, *LPBOOL;
typedef unsigned char BYTE, *PBYTE, *LPBYTE;
typedef unsigned short WORD, *PWORD, *LPWORD;
typedef unsigned int DWORD, *PDWORD, *LPDWORD;
typedef struct _GUID
{
} GUID;
enum
{
  _ISupper = ((0) < 8 ? ((1 << (0)) << 8) : ((1 << (0)) >> 8)),
};
extern char *__strtok_r (char *__restrict __s,
    __const char *__restrict __delim,
         char **__restrict __save_ptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 3)));
__strcspn_c3 (__const char *__s, int __reject1, int __reject2,
       int __reject3)
{
}
extern __inline size_t __strspn_c3 (__const char *__s, int __accept1,
        int __accept2, int __accept3);
extern __inline size_t
__strspn_c3 (__const char *__s, int __accept1, int __accept2, int __accept3)
{
}
extern __inline char *__strpbrk_c2 (__const char *__s, int __accept1,
         int __accept2);
extern __inline char *
__strpbrk_c2 (__const char *__s, int __accept1, int __accept2)
{
}
extern __inline char *__strpbrk_c3 (__const char *__s, int __accept1,
       int __accept3)
{
}
__strtok_r_1c (char *__s, char __sep, char **__nextp)
{
    {
   {
   }
    }
}
__strsep_1c (char **__s, char __reject)
{
}
__strsep_2c (char **__s, char __reject1, char __reject2)
{
    {
 {
 }
    }
}
extern __inline char *__strsep_3c (char **__s, char __reject1, char __reject2,
       char __reject3);
extern __inline char *
__strsep_3c (char **__s, char __reject1, char __reject2, char __reject3)
{
    {
 {
     {
     }
 }
    }
}
__attribute__ ((__nothrow__)) __memcpy_ichk (void *__restrict __dest, __const void *__restrict __src, size_t __len)
{
}
__attribute__ ((__nothrow__)) __memmove_ichk (void *__dest, __const void *__src, size_t __len)
{
}
__attribute__ ((__nothrow__)) __strncpy_ichk (char *__restrict __dest, __const char *__restrict __src, size_t __len)
{
}
__attribute__ ((__nothrow__)) stpncpy (char *__dest, __const char *__src, size_t __n)
{
  if (__builtin_object_size (__dest, 2 > 1) != (size_t) -1
      && (!__builtin_constant_p (__n) || __n <= __builtin_object_size (__dest, 2 > 1)))
    return __stpncpy_chk (__dest, __src, __n, __builtin_object_size (__dest, 2 > 1));
}
__attribute__ ((__nothrow__)) __strncat_ichk (char *__restrict __dest, __const char *__restrict __src, size_t __len)
{
}
typedef void *PVOID;
typedef char CHAR, *PCHAR;
typedef int LONG, *PLONG;
typedef unsigned short WCHAR, *PWCHAR;
typedef CHAR *PSTR, *LPSTR, *NPSTR;
typedef const CHAR *PCSTR, *LPCSTR;
typedef WCHAR *PWSTR, *LPWSTR, *NWPSTR;
typedef LONG HRESULT;
typedef struct _MEMORY_BASIC_INFORMATION
{
} SINGLE_LIST_ENTRY, *PSINGLE_LIST_ENTRY;
typedef enum _HEAP_INFORMATION_CLASS {
    HeapCompatibilityInformation,
} HEAP_INFORMATION_CLASS;
typedef struct _FLOATING_SAVE_AREA
{
} CONTEXT86;
typedef struct _LDT_ENTRY {
    union {
        struct {
        } Bits;
    } HighWord;
} LDT_ENTRY, *PLDT_ENTRY;
typedef struct _EXCEPTION_RECORD
{
} EXCEPTION_RECORD, *PEXCEPTION_RECORD;
typedef struct _EXCEPTION_POINTERS
{
} EXCEPTION_POINTERS, *PEXCEPTION_POINTERS;
typedef struct _NT_TIB
{
 union {
 } ;
} NT_TIB, *PNT_TIB;
extern inline struct _TEB * __attribute__((__stdcall__)) NtCurrentTeb(void)
{
} IMAGE_SECTION_HEADER, *PIMAGE_SECTION_HEADER;
typedef struct _IMAGE_SYMBOL {
    union {
        struct {
        } FcnAry;
    } Section;
} IMAGE_AUX_SYMBOL;
typedef struct _IMAGE_EXPORT_DIRECTORY {
 union {
 } u1;
} IMAGE_BOUND_FORWARDER_REF, *PIMAGE_BOUND_FORWARDER_REF;
typedef struct _IMAGE_BASE_RELOCATION
{
} IMAGE_BASE_RELOCATION,*PIMAGE_BASE_RELOCATION;
typedef struct _IMAGE_RELOCATION
{
    union {
    } ;
} IMAGE_RELOCATION, *PIMAGE_RELOCATION;
typedef struct _IMAGE_ARCHIVE_MEMBER_HEADER
{
} IMAGE_RESOURCE_DIRECTORY,*PIMAGE_RESOURCE_DIRECTORY;
typedef struct _IMAGE_RESOURCE_DIRECTORY_ENTRY {
 union {
  struct {
  } ;
                struct {
  } ;
 } ;
} IMAGE_DEBUG_DIRECTORY, *PIMAGE_DEBUG_DIRECTORY;
typedef enum ReplacesCorHdrNumericDefines
{
    MAX_PACKAGE_NAME = 1024,
} ReplacesCorHdrNumericDefines;
typedef struct IMAGE_COR20_HEADER
{
} MESSAGE_RESOURCE_DATA,*PMESSAGE_RESOURCE_DATA;
typedef PVOID PSECURITY_DESCRIPTOR;
typedef enum _TOKEN_INFORMATION_CLASS {
  TokenUser = 1,
} TOKEN_INFORMATION_CLASS;
typedef struct _GENERIC_MAPPING {
} ACL, *PACL;
typedef struct _ACL_SIZE_INFORMATION
{
} ACL_SIZE_INFORMATION, *PACL_SIZE_INFORMATION;
typedef WORD SECURITY_DESCRIPTOR_CONTROL, *PSECURITY_DESCRIPTOR_CONTROL;
typedef struct {
} SID_AND_ATTRIBUTES;
typedef enum {
    WinBuiltinTerminalServerLicenseServersSid = 60
} WELL_KNOWN_SID_TYPE;
typedef struct _TOKEN_USER {
} TOKEN_GROUPS, *PTOKEN_GROUPS;
typedef union _LARGE_INTEGER {
    struct {
    };
} LARGE_INTEGER, *PLARGE_INTEGER;
typedef union _ULARGE_INTEGER {
    struct {
    };
} LUID, *PLUID;
typedef struct _LUID_AND_ATTRIBUTES {
} LUID_AND_ATTRIBUTES, *PLUID_AND_ATTRIBUTES;
typedef enum tagTOKEN_TYPE {
  TokenPrimary = 1,
} SECURITY_IMPERSONATION_LEVEL, *PSECURITY_IMPERSONATION_LEVEL;
typedef struct _SECURITY_QUALITY_OF_SERVICE {
} QUOTA_LIMITS_EX, *PQUOTA_LIMITS_EX;
typedef enum _LATENCY_TIME {
        VerifyProcessorPowerPolicyDc,
} POWER_INFORMATION_LEVEL;
typedef struct _ADMINISTRATOR_POWER_POLICY {
} RTL_CRITICAL_SECTION_DEBUG, *PRTL_CRITICAL_SECTION_DEBUG, RTL_RESOURCE_DEBUG, *PRTL_RESOURCE_DEBUG;
typedef struct _RTL_CRITICAL_SECTION {
} RTL_CRITICAL_SECTION, *PRTL_CRITICAL_SECTION;
typedef struct _IO_COUNTERS {
} OSVERSIONINFOA, *POSVERSIONINFOA, *LPOSVERSIONINFOA;
typedef struct {
} OSVERSIONINFOW, *POSVERSIONINFOW, *LPOSVERSIONINFOW, RTL_OSVERSIONINFOW, *PRTL_OSVERSIONINFOW;
typedef struct {
} OSVERSIONINFOEXA, *POSVERSIONINFOEXA, *LPOSVERSIONINFOEXA;
typedef struct {
} OSVERSIONINFOEXW, *POSVERSIONINFOEXW, *LPOSVERSIONINFOEXW, RTL_OSVERSIONINFOEXW, *PRTL_OSVERSIONINFOEXW;
typedef struct tagSIZE
{
} POINT, *PPOINT, *LPPOINT;
typedef struct _POINTL
{
} FILETIME, *PFILETIME, *LPFILETIME;
typedef struct tagRECT
{
  } ldiv_t;
extern double strtod (__const char *__restrict __nptr,
        char **__restrict __endptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));
extern long int strtol (__const char *__restrict __nptr,
   char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));
extern long int __strtol_internal (__const char *__restrict __nptr,
       int __base, int __group)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));
extern unsigned long int __strtoul_internal (__const char *__restrict __nptr,
          int __base, int __group)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));
extern __inline double
__attribute__ ((__nothrow__)) strtod (__const char *__restrict __nptr, char **__restrict __endptr)
{
}
extern __inline long int
__attribute__ ((__nothrow__)) strtol (__const char *__restrict __nptr, char **__restrict __endptr, int __base)
{
}
__attribute__ ((__nothrow__)) strtoll (__const char *__restrict __nptr, char **__restrict __endptr, int __base)
{
}
__attribute__ ((__nothrow__)) atoi (__const char *__nptr)
{
}
typedef int int32_t __attribute__ ((__mode__ (__SI__)));
typedef struct
  {
  };
extern int random_r (struct random_data *__restrict __buf,
       int32_t *__restrict __result) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
struct drand48_data
  {
  };
extern int drand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int mrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int jrand48_r (unsigned short int __xsubi[3],
        long int *__restrict __result)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));
extern int seed48_r (unsigned short int __seed16v[3],
       struct drand48_data *__buffer) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *qfcvt (long double __value, int __ndigit,
        size_t __resolvedlen) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));
__attribute__ ((__nothrow__)) realpath (__const char *__restrict __name, char *__restrict __resolved)
{
}
__attribute__ ((__nothrow__)) ptsname_r (int __fd, char *__buf, size_t __buflen)
{
}
typedef struct _EXCEPTION_DEBUG_INFO {
} EXCEPTION_DEBUG_INFO;
typedef struct _CREATE_THREAD_DEBUG_INFO {
} CREATE_THREAD_DEBUG_INFO;
typedef struct _CREATE_PROCESS_DEBUG_INFO {
} CREATE_PROCESS_DEBUG_INFO;
typedef struct _EXIT_THREAD_DEBUG_INFO {
} LOAD_DLL_DEBUG_INFO;
typedef struct _UNLOAD_DLL_DEBUG_INFO {
} RIP_INFO;
typedef struct _DEBUG_EVENT {
    union {
    } u;
} DEBUG_EVENT, *LPDEBUG_EVENT;
typedef struct _OFSTRUCT
{
} WIN32_FIND_DATAA, *PWIN32_FIND_DATAA, *LPWIN32_FIND_DATAA;
typedef struct _WIN32_FIND_DATAW
{
} WIN32_FIND_DATAW, *PWIN32_FIND_DATAW, *LPWIN32_FIND_DATAW;
typedef enum _FINDEX_SEARCH_OPS
{
 FindExSearchNameMatch,
} FINDEX_SEARCH_OPS;
typedef struct _PROCESS_HEAP_ENTRY
{
    union {
        struct {
        } Block;
        struct {
        } Region;
    } ;
} PROCESS_HEAP_ENTRY, *PPROCESS_HEAP_ENTRY, *LPPROCESS_HEAP_ENTRY;
typedef struct tagMEMORYSTATUS
{
} MEMORYSTATUSEX, *LPMEMORYSTATUSEX;
typedef struct _SYSTEMTIME{
        WORD wYear;
} SYSTEMTIME, *PSYSTEMTIME, *LPSYSTEMTIME;
typedef struct _OVERLAPPED {
        union {
            struct {
            } ;
        } ;
} PROCESS_INFORMATION, *PPROCESS_INFORMATION, *LPPROCESS_INFORMATION;
typedef struct _TIME_ZONE_INFORMATION{
} TIME_ZONE_INFORMATION, *PTIME_ZONE_INFORMATION, *LPTIME_ZONE_INFORMATION;
typedef struct _BY_HANDLE_FILE_INFORMATION
{
} ACTCTX_SECTION_KEYED_DATA_ASSEMBLY_METADATA, *PACTCTX_SECTION_KEYED_DATA_ASSEMBLY_METADATA;
typedef struct tagACTCTX_SECTION_KEYED_DATA {
} COMMTIMEOUTS,*LPCOMMTIMEOUTS;
typedef enum _COMPUTER_NAME_FORMAT
{
 ComputerNameNetBIOS,
} COMPUTER_NAME_FORMAT;
typedef struct tagHW_PROFILE_INFOA {
} HW_PROFILE_INFOA, *LPHW_PROFILE_INFOA;
typedef struct tagHW_PROFILE_INFOW {
} HW_PROFILE_INFOW, *LPHW_PROFILE_INFOW;
BOOL __attribute__((__stdcall__)) SetSecurityDescriptorControl(PSECURITY_DESCRIPTOR,SECURITY_DESCRIPTOR_CONTROL,
                                                SECURITY_DESCRIPTOR_CONTROL);
typedef struct tagSYSLEVEL
{
} SYSLEVEL;
static inline PVOID __attribute__((__stdcall__)) InterlockedCompareExchangePointer( PVOID volatile *dest, PVOID xchg, PVOID compare )
{
}
static inline PVOID __attribute__((__stdcall__)) InterlockedExchangePointer( PVOID volatile *dest, PVOID val )
{
}
typedef unsigned long HCRYPTPROV;
typedef unsigned long HCRYPTKEY;
typedef void *HCERTSTOREPROV;
typedef struct _PROV_ENUMALGS {
} PROV_ENUMALGS;
typedef struct _HMAC_INFO {
} HMAC_INFO, *PHMAC_INFO;
typedef struct _CRYPTOAPI_BLOB {
  DWORD cbData;
  BYTE* pbData;
} CRYPT_INTEGER_BLOB, *PCRYPT_INTEGER_BLOB,
  CRYPT_OBJID_BLOB, *PCRYPT_OBJID_BLOB,
  CERT_NAME_BLOB, *PCERT_NAME_BLOB,
  CERT_RDN_VALUE_BLOB, *PCERT_RDN_VALUE_BLOB,
  CRYPT_DER_BLOB, *PCRYPT_DER_BLOB,
  CRYPT_ATTR_BLOB, *PCRYPT_ATTR_BLOB;
typedef struct _CRYPTPROTECT_PROMPTSTRUCT{
} CRYPTPROTECT_PROMPTSTRUCT, *PCRYPTPROTECT_PROMPTSTRUCT;
typedef struct _CRYPT_ALGORITHM_IDENTIFIER {
  LPSTR pszObjId;
  CRYPT_OBJID_BLOB Parameters;
} CRYPT_ALGORITHM_IDENTIFIER, *PCRYPT_ALGORITHM_IDENTIFIER;
typedef struct _CRYPT_ATTRIBUTE_TYPE_VALUE {
} CRYPT_ATTRIBUTE_TYPE_VALUE, *PCRYPT_ATTRIBUTE_TYPE_VALUE;
typedef struct _PUBLICKEYSTRUC {
} BLOBHEADER, PUBLICKEYSTRUC;
typedef struct _RSAPUBKEY {
    DWORD magic;
    DWORD pubexp;
} RSAPUBKEY;
typedef struct _CRYPT_BIT_BLOB {
    DWORD cbData;
    BYTE *pbData;
    DWORD cUnusedBits;
} CRYPT_BIT_BLOB, *PCRYPT_BIT_BLOB;
typedef struct _CRYPT_KEY_PROV_PARAM {
} CRYPT_KEY_PROV_PARAM, *PCRYPT_KEY_PROV_PARAM;
typedef struct _CRYPT_KEY_PROV_INFO {
    CRYPT_ALGORITHM_IDENTIFIER Algorithm;
    CRYPT_BIT_BLOB PublicKey;
} CERT_PUBLIC_KEY_INFO, *PCERT_PUBLIC_KEY_INFO;
typedef struct _CERT_EXTENSION {
    LPSTR pszObjId;
    CRYPT_OBJID_BLOB Value;
} CERT_EXTENSION, *PCERT_EXTENSION;
typedef struct _CERT_EXTENSIONS {
    DWORD cExtension;
    PCERT_EXTENSION rgExtension;
} CERT_EXTENSIONS, *PCERT_EXTENSIONS;
typedef struct _CERT_INFO {
    CRYPT_INTEGER_BLOB SerialNumber;
    CERT_NAME_BLOB Subject;
    CERT_PUBLIC_KEY_INFO SubjectPublicKeyInfo;
} CERT_INFO, *PCERT_INFO;
typedef struct _CERT_RDN_ATTR {
    LPSTR pszObjId;
    CERT_RDN_VALUE_BLOB Value;
} CERT_RDN_ATTR, *PCERT_RDN_ATTR;
typedef struct _CERT_RDN {
} CERT_RDN, *PCERT_RDN;
typedef struct _CERT_NAME_INFO {
    DWORD cRDN;
} CERT_NAME_INFO, *PCERT_NAME_INFO;
typedef struct _CERT_NAME_VALUE {
    DWORD dwValueType;
    CERT_RDN_VALUE_BLOB Value;
} CERT_NAME_VALUE, *PCERT_NAME_VALUE;
typedef struct _CERT_ENCRYPTED_PRIVATE_KEY_INFO {
    CERT_NAME_BLOB CertIssuer;
} CERT_AUTHORITY_KEY_ID_INFO, *PCERT_AUTHORITY_KEY_ID_INFO;
typedef struct _CERT_PRIVATE_KEY_VALIDITY {
} CERT_OTHER_NAME, *PCERT_OTHER_NAME;
typedef struct _CERT_ALT_NAME_ENTRY {
    DWORD dwAltNameChoice;
    union {
        LPWSTR pwszURL;
    } ;
} CERT_ALT_NAME_ENTRY, *PCERT_ALT_NAME_ENTRY;
typedef struct _CERT_ALT_NAME_INFO {
    DWORD cAltEntry;
    PCERT_ALT_NAME_ENTRY rgAltEntry;
} CERT_ALT_NAME_INFO, *PCERT_ALT_NAME_INFO;
typedef struct _CERT_BASIC_CONSTRAINTS_INFO {
    CERT_NAME_BLOB *rgSubtreesConstraint;
} CERT_BASIC_CONSTRAINTS_INFO, *PCERT_BASIC_CONSTRAINTS_INFO;
typedef struct _CERT_BASIC_CONSTRAINTS2_INFO {
} CERT_BASIC_CONSTRAINTS2_INFO, *PCERT_BASIC_CONSTRAINTS2_INFO;
typedef struct _CERT_POLICY_QUALIFIER_INFO {
} CERT_POLICY_QUALIFIER_INFO, *PCERT_POLICY_QUALIFIER_INFO;
typedef struct _CERT_POLICY_QUALIFIER_NOTICE_REFERENCE {
} CERT_POLICY_QUALIFIER_NOTICE_REFERENCE,
 *PCERT_POLICY_QUALIFIER_NOTICE_REFERENCE;
typedef struct _CERT_POLICY_QUALIFIER_USER_NOTICE {
    DWORD cValue;
    PCRYPT_DER_BLOB rgValue;
} CRYPT_SEQUENCE_OF_ANY, *PCRYPT_SEQUENCE_OF_ANY;
typedef struct _CERT_AUTHORITY_KEY_ID2_INFO {
    PCERT_INFO pCertInfo;
} CERT_CONTEXT, *PCERT_CONTEXT;
typedef const CERT_CONTEXT *PCCERT_CONTEXT;
typedef struct _CRL_ENTRY {
} CRL_ENTRY, *PCRL_ENTRY;
typedef struct _CRL_INFO {
    DWORD cCRLEntry;
    DWORD cExtension;
} CRL_INFO, *PCRL_INFO;
typedef struct _CRL_DIST_POINT_NAME {
    DWORD dwDistPointNameChoice;
    union {
    } ;
} CRL_DIST_POINT_NAME, *PCRL_DIST_POINT_NAME;
typedef struct _CRL_DIST_POINT {
    CRL_DIST_POINT_NAME DistPointName;
    CRYPT_BIT_BLOB ReasonFlags;
    CERT_ALT_NAME_INFO CRLIssuer;
} CRL_DIST_POINT, *PCRL_DIST_POINT;
typedef struct _CRL_DIST_POINTS_INFO {
    DWORD cDistPoint;
    PCRL_DIST_POINT rgDistPoint;
} CRL_DIST_POINTS_INFO, *PCRL_DIST_POINTS_INFO;
typedef struct _CRL_ISSUING_DIST_POINT {
    BOOL fOnlyContainsUserCerts;
} CRL_ISSUING_DIST_POINT, *PCRL_ISSUING_DIST_POINT;
typedef struct _CERT_GENERAL_SUBTREE {
} CRYPT_ATTRIBUTE, *PCRYPT_ATTRIBUTE;
typedef struct _CRYPT_ATTRIBUTES {
} CRYPT_ATTRIBUTES, *PCRYPT_ATTRIBUTES;
typedef struct _CERT_REQUEST_INFO {
    DWORD dwVersion;
    CRYPT_ALGORITHM_IDENTIFIER SignatureAlgorithm;
    CRYPT_BIT_BLOB Signature;
} CERT_SIGNED_CONTENT_INFO, *PCERT_SIGNED_CONTENT_INFO;
typedef struct _CRL_CONTEXT {
} CRL_CONTEXT, *PCRL_CONTEXT;
typedef struct _VTableProvStruc {
} CTL_CONTEXT, *PCTL_CONTEXT;
typedef struct _CRYPT_TIME_STAMP_REQUEST_INFO {
} CERT_REVOCATION_STATUS, *PCERT_REVOCATION_STATUS;
typedef struct _CERT_TRUST_STATUS {
} CERT_SYSTEM_STORE_RELOCATE_PARA, *PCERT_SYSTEM_STORE_RELOCATE_PARA;
typedef BOOL (__attribute__((__stdcall__)) *PFN_CERT_ENUM_SYSTEM_STORE_LOCATION)(
 void *pvArg);
typedef struct _CRYPT_ENCODE_PARA {
} CERT_STORE_PROV_INFO, *PCERT_STORE_PROV_INFO;
typedef BOOL (__attribute__((__stdcall__)) *PFN_CERT_DLL_OPEN_STORE_PROV_FUNC)(
 DWORD dwFlags, DWORD dwCtrlType, void const *pvCtrlPara);
typedef struct _CERT_STORE_PROV_FIND_INFO {
} CERT_STORE_PROV_FIND_INFO, *PCERT_STORE_PROV_FIND_INFO;
typedef BOOL (__attribute__((__stdcall__)) *PFN_CERT_STORE_PROV_FIND_CERT)(HCERTSTOREPROV hStoreProv,
 DWORD dwFlags, void **ppvStoreProvFindInfo, PCCERT_CONTEXT *ppProvCertContext);
typedef BOOL (__attribute__((__stdcall__)) *PFN_CERT_STORE_PROV_FREE_FIND_CERT)(
 DWORD dwFlags, void *pvData, DWORD *pcbData);
typedef BOOL (__attribute__((__stdcall__)) *PFN_CERT_STORE_PROV_GET_CTL_PROPERTY)(
 DWORD dwFlags, void *pvData);
typedef struct _CERT_CREATE_CONTEXT_PARA {
} CRYPT_OID_FUNC_ENTRY, *PCRYPT_OID_FUNC_ENTRY;
typedef BOOL (__attribute__((__stdcall__)) *PFN_CRYPT_ENUM_OID_FUNC)(DWORD dwEncodingType,
 const DWORD rgcbValueData[], void *pvArg);
typedef struct _CRYPT_OID_INFO {
    union {
    } ;
} CRYPT_OID_INFO, *PCRYPT_OID_INFO;
typedef const CRYPT_OID_INFO CCRYPT_OID_INFO, *PCCRYPT_OID_INFO;
typedef BOOL (__attribute__((__stdcall__)) *PFN_CRYPT_ENUM_OID_INFO)(PCCRYPT_OID_INFO pInfo,
 void *pvArg);
typedef struct _CRYPT_SIGN_MESSAGE_PARA {
} CRYPT_HASH_MESSAGE_PARA, *PCRYPT_HASH_MESSAGE_PARA;
typedef struct _CRYPT_KEY_SIGN_MESSAGE_PARA {
} CRYPT_URL_INFO, *PCRYPT_URL_INFO;
typedef void (__attribute__((__stdcall__)) *PFN_CRYPT_ASYNC_PARAM_FREE_FUNC)(LPSTR pszParamOid,
 LPVOID pvParam);
typedef struct _CRYPT_CREDENTIALS {
} CRYPT_CREDENTIALS, *PCRYPT_CREDENTIALS;
typedef struct _CRYPT_PASSWORD_CREDENTIALSA {
} CRYPT_PASSWORD_CREDENTIALSW, *PCRYPT_PASSWORD_CREDENTIALSW;
typedef struct _CRYPT_RETRIEVE_AUX_INFO {
} CRYPT_RETRIEVE_AUX_INFO, *PCRYPT_RETRIEVE_AUX_INFO;
typedef struct _CERT_CHAIN_ENGINE_CONFIG
{
} CERT_CHAIN_ENGINE_CONFIG, *PCERT_CHAIN_ENGINE_CONFIG;
BOOL __attribute__((__stdcall__)) CryptExportPublicKeyInfoEx(HCRYPTPROV hCryptProv, DWORD dwKeySpec,
 DWORD dwCertEncodingType, LPSTR pszPublicKeyObjId, DWORD dwFlags,
 void *pvAuxInfo, PCERT_PUBLIC_KEY_INFO pInfo, DWORD *pcbInfo);
BOOL __attribute__((__stdcall__)) CryptImportPublicKeyInfo(HCRYPTPROV hCryptProv,
 PCRYPT_RETRIEVE_AUX_INFO pAuxInfo);
struct encodedInt
{
    int val;
    const BYTE *encoded;
};
static const struct encodedInt ints[] = {
};
struct encodedBigInt
{
    const BYTE *val;
    const BYTE *encoded;
};
static const struct encodedBigInt bigInts[] = {
};
static const struct encodedBigInt bigUInts[] = {
};
static void test_encodeInt(DWORD dwEncoding)
{
    DWORD bufSize = 0;
    int i;
    BOOL ret;
    CRYPT_INTEGER_BLOB blob;
    BYTE *buf = ((void *)0);
    ret = CryptEncodeObjectEx(0, ((LPCSTR)27), &ints[0].val, 0, ((void *)0), ((void *)0),
     "Expected STATUS_ACCESS_VIOLATION, got %08x\n", GetLastError());
    {
        ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)27), &ints[i].val,
         0x08000, ((void *)0), &buf, &bufSize);
        {
            (winetest_set_location("encode.c", 119), 0) ? 0 : winetest_ok(buf[0] == 2, "Got unexpected type %d for integer (expected 2)\n",
             buf[1], ints[i].encoded[1]);
        }
        ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)28), &blob,
         0, ((void *)0), ((void *)0), &bufSize);
        {
        }
        ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)38), &blob,
         0x08000, ((void *)0), &buf, &bufSize);
        {
            (winetest_set_location("encode.c", 187), 0) ? 0 : winetest_ok(buf[1] == bigUInts[i].encoded[1], "Got length %d, expected %d\n",
             buf[1], bigUInts[i].encoded[1]);
            (winetest_set_location("encode.c", 189), 0) ? 0 : winetest_ok(!memcmp(buf + 1, bigUInts[i].encoded + 1,
             bigUInts[i].encoded[1] + 1),
             "Encoded value didn't match expected\n");
        }
    }
}
static void test_decodeInt(DWORD dwEncoding)
{
    static const BYTE longForm[] = { 2, 0x81, 0x01, 0x01 };
    BYTE *buf = ((void *)0);
    DWORD bufSize = 0;
    int i;
    BOOL ret;
    ret = CryptDecodeObjectEx(3, ((LPCSTR)27), (BYTE *)&ints[0].encoded,
     ints[0].encoded[1] + 2, 0, ((void *)0), ((void *)0), &bufSize);
    (winetest_set_location("encode.c", 225), 0) ? 0 : winetest_ok(!ret && GetLastError() == ((HRESULT)0x80093104L),
     "Expected CRYPT_E_ASN1_BADTAG, got %d\n", GetLastError());
    {
        {
            (winetest_set_location("encode.c", 249), 0) ? 0 : winetest_ok(!memcmp(buf, &ints[i].val, bufSize), "Expected %d, got %d\n",
             ints[i].val, *(int *)buf);
        }
    }
    {
        ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)28),
         &bufSize);
        {
            CRYPT_INTEGER_BLOB *blob = (CRYPT_INTEGER_BLOB *)buf;
            (winetest_set_location("encode.c", 296), 0) ? 0 : winetest_ok(blob->cbData == strlen((const char*)bigUInts[i].val),
             "Unexpected value\n");
        }
    }
    ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)28), longForm,
     sizeof(longForm), 0x08000, ((void *)0), (BYTE *)&buf, &bufSize);
    {
    }
}
static const struct encodedInt enums[] = {
};
static const LPCSTR enumeratedTypes[] = { ((LPCSTR)29),
 "2.5.29.21" };
static void test_encodeEnumerated(DWORD dwEncoding)
{
    DWORD i, j;
    {
        {
            BOOL ret;
            DWORD bufSize = 0;
            ret = CryptEncodeObjectEx(dwEncoding, enumeratedTypes[i],
             &bufSize);
            {
            }
        }
    }
}
static void test_decodeEnumerated(DWORD dwEncoding)
{
    DWORD i, j;
    {
        {
            BOOL ret;
            DWORD bufSize = sizeof(int);
            int val;
            ret = CryptDecodeObjectEx(dwEncoding, enumeratedTypes[i],
             (BYTE *)&val, &bufSize);
            (winetest_set_location("encode.c", 403), 0) ? 0 : winetest_ok(val == enums[j].val, "Unexpected value %d, expected %d\n",
             val, enums[j].val);
        }
    }
}
struct encodedFiletime
{
    SYSTEMTIME sysTime;
    const BYTE *encodedTime;
};
static void testTimeEncoding(DWORD dwEncoding, LPCSTR structType,
 const struct encodedFiletime *time)
{
}
static void testTimeDecoding(DWORD dwEncoding, LPCSTR structType,
 const struct encodedFiletime *time)
{
    BOOL ret;
    if (structType == ((LPCSTR)30) ||
     (time->sysTime.wYear >= 1950 && time->sysTime.wYear <= 2050))
    {
    }
        (winetest_set_location("encode.c", 476), 0) ? 0 : winetest_ok(!ret && GetLastError() == ((HRESULT)0x8009310BL),
         "Expected CRYPT_E_ASN1_BADTAG, got 0x%08x\n", GetLastError());
}
static const BYTE bin22[] = {
    0x18,0x0f,'2','1','4','5','0','6','0','6','1','6','1','0','0','0','Z'};
static const struct encodedFiletime times[] = {
};
static void test_encodeFiletime(DWORD dwEncoding)
{
    {
    }
}
static const BYTE bin23[] = {
    0x18,0x13,'1','9','4','5','0','6','0','6','1','6','1','0','0','0','.','0','0','0','Z'};
static const BYTE bin24[] = {
    0x18,0x13,'1','9','4','5','0','6','0','6','1','6','1','0','0','0','.','9','9','9','Z'};
static const BYTE bin26[] = {
    0x17,0x0b,'4','5','0','6','0','6','1','6','1','0','Z'};
static const BYTE bin33[] = {
    0x17,0x0f,'4','5','0','6','0','6','1','6','1','0','-','0','1','0','0'};
static const BYTE bin35[] = {
    0x17,0x08, '4','5','0','6','0','6','1','6'};
static const BYTE bin38[] = {
    0x18,0x08, '2','1','4','5','0','6','0','6'};
static void test_decodeFiletime(DWORD dwEncoding)
{
    static const struct encodedFiletime otherTimes[] = {
    };
    static const unsigned char *bogusTimes[] = {
    };
    {
    }
}
static const char commonName[] = "Juan Lang";
static const char surName[] = "Lang";
static const BYTE emptySequence[] = { 0x30, 0 };
static const BYTE emptyRDNs[] = { 0x30, 0x02, 0x31, 0 };
static const BYTE twoRDNs[] = {
    0x13,0x0a,0x4a,0x75,0x61,0x6e,0x20,0x4c,0x61,0x6e,0x67,0};
static const BYTE encodedTwoRDNs[] = {
};
static const BYTE us[] = { 0x55, 0x53 };
static const BYTE minnesota[] = { 0x4d, 0x69, 0x6e, 0x6e, 0x65, 0x73, 0x6f,
 0x6f, 0x6c, 0x69, 0x73 };
static const BYTE codeweavers[] = { 0x43, 0x6f, 0x64, 0x65, 0x57, 0x65, 0x61,
 0x76, 0x65, 0x72, 0x73 };
static const BYTE wine[] = { 0x57, 0x69, 0x6e, 0x65, 0x20, 0x44, 0x65, 0x76,
 0x65, 0x6c, 0x6f, 0x70, 0x6d, 0x65, 0x6e, 0x74 };
static const BYTE aric[] = { 0x61, 0x72, 0x69, 0x63, 0x40, 0x63, 0x6f, 0x64,
 0x65, 0x77, 0x65, 0x61, 0x76, 0x65, 0x72, 0x73, 0x2e, 0x63, 0x6f, 0x6d };
static CHAR oid_us[] = "2.5.4.6",
            oid_aric[] = "1.2.840.113549.1.9.1";
static CERT_RDN_ATTR rdnAttrs[] = { { oid_us, 4, { sizeof(us), (LPBYTE)us } },
                                           { oid_aric, 7, { sizeof(aric), (LPBYTE)aric } } };
static const BYTE encodedRDNAttrs[] = {
};
static void test_encodeName(DWORD dwEncoding)
{
    CERT_NAME_INFO info;
    static CHAR oid_common_name[] = "2.5.4.3",
                oid_sur_name[] = "2.5.4.4";
    BOOL ret;
    ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)7), ((void *)0),
     "Expected STATUS_ACCESS_VIOLATION, got %08x\n", GetLastError());
    {
    }
    ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)7), &info,
     "Expected STATUS_ACCESS_VIOLATION, got %08x\n", GetLastError());
    {
    }
    {
    }
    ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)7), &info,
     "Expected E_INVALIDARG, got %08x\n", GetLastError());
    {
    }
}
static WCHAR commonNameW[] = { 'J','u','a','n',' ','L','a','n','g',0 };
static const BYTE twoRDNsNoNull[] = {
 0x20,0x4c,0x61,0x6e,0x67 };
static const BYTE anyType[] = {
 0x61,0x4c,0x67,0x6e };
static void test_encodeUnicodeName(DWORD dwEncoding)
{
    BOOL ret;
    ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)20), ((void *)0),
     "Expected STATUS_ACCESS_VIOLATION, got %08x\n", GetLastError());
    {
    }
}
static void compareNameValues(const CERT_NAME_VALUE *expected,
 const CERT_NAME_VALUE *got)
{
    (winetest_set_location("encode.c", 913), 0) ? 0 : winetest_ok(got->dwValueType == expected->dwValueType,
     expected->dwValueType, got->Value.cbData, expected->Value.cbData);
        (winetest_set_location("encode.c", 920), 0) ? 0 : winetest_ok(!memcmp(got->Value.pbData, expected->Value.pbData,
         (((got->Value.cbData) < (expected->Value.cbData)) ? (got->Value.cbData) : (expected->Value.cbData))),
         "String type %d: unexpected value\n", expected->dwValueType);
}
static void compareRDNAttrs(const CERT_RDN_ATTR *expected,
 const CERT_RDN_ATTR *got)
{
    {
        {
            (winetest_set_location("encode.c", 934), 0) ? 0 : winetest_ok(!__extension__ ({ size_t __s1_len, __s2_len; (__builtin_constant_p (got->pszObjId) && __builtin_constant_p (expected->pszObjId) && (__s1_len = strlen (got->pszObjId), __s2_len = strlen (expected->pszObjId), (!((size_t)(const void *)((got->pszObjId) + 1) - (size_t)(const void *)(got->pszObjId) == 1) || __s1_len >= 4) && (!((size_t)(const void *)((expected->pszObjId) + 1) - (size_t)(const void *)(expected->pszObjId) == 1) || __s2_len >= 4)) ? __builtin_strcmp (got->pszObjId, expected->pszObjId) : (__builtin_constant_p (got->pszObjId) && ((size_t)(const void *)((got->pszObjId) + 1) - (size_t)(const void *)(got->pszObjId) == 1) && (__s1_len = strlen (got->pszObjId), __s1_len < 4) ? (__builtin_constant_p (expected->pszObjId) && ((size_t)(const void *)((expected->pszObjId) + 1) - (size_t)(const void *)(expected->pszObjId) == 1) ? __builtin_strcmp (got->pszObjId, expected->pszObjId) : (__extension__ ({ __const unsigned char *__s2 = (__const unsigned char *) (__const char *) (expected->pszObjId); register int __result = (((__const unsigned char *) (__const char *) (got->pszObjId))[0] - __s2[0]); if (__s1_len > 0 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (got->pszObjId))[1] - __s2[1]); if (__s1_len > 1 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (got->pszObjId))[2] - __s2[2]); if (__s1_len > 2 && __result == 0) __result = (((__const unsigned char *) (__const char *) (got->pszObjId))[3] - __s2[3]); } } __result; }))) : (__builtin_constant_p (expected->pszObjId) && ((size_t)(const void *)((expected->pszObjId) + 1) - (size_t)(const void *)(expected->pszObjId) == 1) && (__s2_len = strlen (expected->pszObjId), __s2_len < 4) ? (__builtin_constant_p (got->pszObjId) && ((size_t)(const void *)((got->pszObjId) + 1) - (size_t)(const void *)(got->pszObjId) == 1) ? __builtin_strcmp (got->pszObjId, expected->pszObjId) : (__extension__ ({ __const unsigned char *__s1 = (__const unsigned char *) (__const char *) (got->pszObjId); register int __result = __s1[0] - ((__const unsigned char *) (__const char *) (expected->pszObjId))[0]; if (__s2_len > 0 && __result == 0) { __result = (__s1[1] - ((__const unsigned char *) (__const char *) (expected->pszObjId))[1]); if (__s2_len > 1 && __result == 0) { __result = (__s1[2] - ((__const unsigned char *) (__const char *) (expected->pszObjId))[2]); if (__s2_len > 2 && __result == 0) __result = (__s1[3] - ((__const unsigned char *) (__const char *) (expected->pszObjId))[3]); } } __result; }))) : __builtin_strcmp (got->pszObjId, expected->pszObjId)))); }),
             expected->pszObjId);
        }
    }
}
static void compareRDNs(const CERT_RDN *expected, const CERT_RDN *got)
{
    {
    }
}
static void compareNames(const CERT_NAME_INFO *expected,
 const CERT_NAME_INFO *got)
{
    (winetest_set_location("encode.c", 959), 0) ? 0 : winetest_ok(got->cRDN == expected->cRDN, "Expected %d RDNs, got %d\n",
     expected->cRDN, got->cRDN);
    {
    }
}
static void test_decodeName(DWORD dwEncoding)
{
    BYTE *buf = ((void *)0);
    DWORD bufSize = 0;
    BOOL ret;
    ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)7), emptySequence,
     (BYTE *)&buf, &bufSize);
    {
        static CHAR oid_sur_name[] = "2.5.4.4",
                    oid_common_name[] = "2.5.4.3";
        CERT_RDN_ATTR attrs[] = {
         { oid_sur_name, 4, { sizeof(surName),
          (BYTE *)commonName } },
        };
    }
    {
    }
}
static void test_decodeUnicodeName(DWORD dwEncoding)
{
    BYTE *buf = ((void *)0);
    DWORD bufSize = 0;
    BOOL ret;
    ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)20), emptySequence,
     (BYTE *)&buf, &bufSize);
    {
    }
    ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)20), emptyRDNs,
     (BYTE *)&buf, &bufSize);
    {
    }
    ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)20), twoRDNsNoNull,
     (BYTE *)&buf, &bufSize);
    {
        static CHAR oid_sur_name[] = "2.5.4.4",
                    oid_common_name[] = "2.5.4.3";
        CERT_RDN_ATTR attrs[] = {
         { oid_sur_name, 4,
         { lstrlenW(commonNameW) * sizeof(WCHAR), (BYTE *)commonNameW } },
        };
    }
}
struct EncodedNameValue
{
    CERT_NAME_VALUE value;
};
static const char bogusPrintable[] = "~";
static const BYTE bin42[] = { 0x16,0x02,0x80,0x00 };
static const BYTE bin43[] = { 0x13,0x02,0x7e,0x00 };
static BYTE octetCommonNameValue[] = {
 0x12,0x0a,0x4a,0x75,0x61,0x6e,0x20,0x4c,0x61,0x6e,0x67,0x00 };
static BYTE printableCommonNameValue[] = {
 0x13,0x0a,0x4a,0x75,0x61,0x6e,0x20,0x4c,0x61,0x6e,0x67,0x00 };
static BYTE t61CommonNameValue[] = {
 0x14,0x0a,0x4a,0x75,0x61,0x6e,0x20,0x4c,0x61,0x6e,0x67,0x00 };
static BYTE graphicCommonNameValue[] = {
 0x61,0x00,0x6e,0x00,0x67,0x00,0x00 };
static BYTE utf8CommonNameValue[] = {
 0x0c,0x0a,0x4a,0x75,0x61,0x6e,0x20,0x4c,0x61,0x6e,0x67,0x00 };
static struct EncodedNameValue nameValues[] = {
 { { 5, { sizeof(commonName), (BYTE *)commonName } },
     sizeof(bin42) },
 { { 4, { sizeof(bogusPrintable),
     (BYTE *)bogusPrintable } }, bin43, sizeof(bin43) },
};
static void test_encodeNameValue(DWORD dwEncoding)
{
    DWORD size = 0, i;
    BOOL ret;
    CERT_NAME_VALUE value = { 0, { 0, ((void *)0) } };
    ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)6), &value,
     "Expected CRYPT_E_ASN1_CHOICE, got %08x\n", GetLastError());
    {
        (winetest_set_location("encode.c", 1209), 0) ? 0 : winetest_ok(size == sizeof(printableCommonNameValue), "Unexpected size %d\n",
         "Unexpected encoding\n");
        ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)6),
         nameValues[i].value.dwValueType, GetLastError());
        {
        }
    }
}
static void test_decodeNameValue(DWORD dwEncoding)
{
    int i;
    BYTE *buf = ((void *)0);
    DWORD bufSize = 0;
    BOOL ret;
    {
        ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)6),
         (BYTE *)&buf, &bufSize);
        {
            compareNameValues(&nameValues[i].value,
             (const CERT_NAME_VALUE *)buf);
        }
    }
}
static const BYTE emptyURL[] = { 0x30, 0x02, 0x86, 0x00 };
static const WCHAR url[] = { 'h','t','t','p',':','/','/','w','i','n','e',
 0x6f, 0x72, 0x67 };
static const BYTE encodedIPAddr[] = { 0x30, 0x06, 0x87, 0x04, 0x7f, 0x00, 0x00,
 0x01 };
static void test_encodeAltName(DWORD dwEncoding)
{
    CERT_ALT_NAME_INFO info = { 0 };
    BYTE *buf = ((void *)0);
    DWORD size = 0;
    BOOL ret;
    ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)12), &info,
     0x08000, ((void *)0), (BYTE *)&buf, &size);
    {
    }
    {
    }
}
static void test_decodeAltName(DWORD dwEncoding)
{
    BOOL ret;
    DWORD bufSize = 0;
    CERT_ALT_NAME_INFO *info;
    ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)12),
     &bufSize);
    {
        (winetest_set_location("encode.c", 1392), 0) ? 0 : winetest_ok(info->cAltEntry == 0, "Expected 0 entries, got %d\n",
         info->cAltEntry);
    }
    ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)12), emptyURL,
     &bufSize);
    {
    }
}
struct encodedBits
{
    const BYTE *encoded;
    DWORD cbDecoded;
};
static const struct encodedBits bits[] = {
};
static void test_encodeBits(DWORD dwEncoding)
{
    DWORD i;
    {
        DWORD bufSize = 0;
        {
            (winetest_set_location("encode.c", 1802), 0) ? 0 : winetest_ok(bufSize == bits[i].encoded[1] + 2,
             bits[i].encoded[1] + 2);
        }
    }
}
static void test_decodeBits(DWORD dwEncoding)
{
    DWORD i;
    {
        {
            CRYPT_BIT_BLOB *blob;
            (winetest_set_location("encode.c", 1835), 0) ? 0 : winetest_ok(blob->cbData == bits[i].cbDecoded,
                 "Unexpected value\n");
        }
    }
    {
    }
}
struct Constraints2
{
    CERT_BASIC_CONSTRAINTS2_INFO info;
};
static const struct Constraints2 constraints2[] = {
};
static const BYTE encodedDomainName[] = { 0x30, 0x2b, 0x31, 0x29, 0x30, 0x11,
 0x16, 0x06, 0x77, 0x69, 0x6e, 0x65, 0x68, 0x71 };
static void test_encodeBasicConstraints(DWORD dwEncoding)
{
    CERT_NAME_BLOB nameBlob = { sizeof(encodedDomainName),
     (LPBYTE)encodedDomainName };
    {
        {
        }
    }
    {
    }
}
static const unsigned char encodedCommonName[] = {
    0x30,0x15,0x31,0x13,0x30,0x11,0x06,0x03,0x55,0x04,0x03,0x13,0x0a,'J','u','a','n',' ','L','a','n','g',0};
static void test_decodeBasicConstraints(DWORD dwEncoding)
{
    static const BYTE inverted[] = { 0x30, 0x06, 0x02, 0x01, 0x01, 0x01, 0x01,
     0xff };
    DWORD i;
    BOOL ret;
    BYTE *buf = ((void *)0);
    DWORD bufSize = 0;
    {
        ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)15),
         0x08000, ((void *)0), (BYTE *)&buf, &bufSize);
        {
            CERT_BASIC_CONSTRAINTS2_INFO *info =
            (winetest_set_location("encode.c", 1984), 0) ? 0 : winetest_ok(!memcmp(info, &constraints2[i].info, sizeof(*info)),
             "Unexpected value for item %d\n", i);
        }
    }
    ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)15),
     (BYTE *)&buf, &bufSize);
    {
        CERT_BASIC_CONSTRAINTS_INFO *info = (CERT_BASIC_CONSTRAINTS_INFO *)buf;
        {
            (winetest_set_location("encode.c", 2043), 0) ? 0 : winetest_ok(info->rgSubtreesConstraint[0].cbData ==
             info->rgSubtreesConstraint[0].cbData);
            (winetest_set_location("encode.c", 2046), 0) ? 0 : winetest_ok(!memcmp(info->rgSubtreesConstraint[0].pbData, encodedDomainName,
             sizeof(encodedDomainName)), "Unexpected value\n");
        }
    }
}
static const BYTE modulus1[] = { 0,0,0,1,1,1,1,1 };
struct EncodedRSAPubKey
{
    const BYTE *modulus;
    size_t decodedModulusLen;
};
struct EncodedRSAPubKey rsaPubKeys[] = {
};
static void test_encodeRsaPublicKey(DWORD dwEncoding)
{
    BYTE toEncode[sizeof(BLOBHEADER) + sizeof(RSAPUBKEY) + sizeof(modulus1)];
    RSAPUBKEY *rsaPubKey = (RSAPUBKEY *)(toEncode + sizeof(BLOBHEADER));
    BOOL ret;
    BYTE *buf = ((void *)0);
    DWORD bufSize = 0, i;
    ret = CryptDecodeObjectEx(dwEncoding, "1.2.840.113549.1.1.1",
     "Expected ERROR_FILE_NOT_FOUND, got %08x\n", GetLastError());
    {
        {
            (winetest_set_location("encode.c", 2210), 0) ? 0 : winetest_ok(bufSize >= sizeof(BLOBHEADER) + sizeof(RSAPUBKEY) +
             "Wrong size %d\n", bufSize);
            (winetest_set_location("encode.c", 2223), 0) ? 0 : winetest_ok(rsaPubKey->magic == 0x31415352,
             rsaPubKey->pubexp);
            (winetest_set_location("encode.c", 2229), 0) ? 0 : winetest_ok(!memcmp(buf + sizeof(BLOBHEADER) + sizeof(RSAPUBKEY),
             rsaPubKeys[i].modulus, rsaPubKeys[i].decodedModulusLen),
             "Unexpected modulus\n");
        }
    }
}
static const BYTE intSequence[] = { 0x30, 0x1b, 0x02, 0x01, 0x01, 0x02, 0x01,
 0x02, 0xff, 0x7f, 0x02, 0x04, 0xba, 0xdd, 0xf0, 0x0d };
static const BYTE mixedSequence[] = { 0x30, 0x27, 0x17, 0x0d, 0x30, 0x35, 0x30,
 0xff, 0x7f, 0x02, 0x04, 0xba, 0xdd, 0xf0, 0x0d };
static void test_encodeSequenceOfAny(DWORD dwEncoding)
{
    BYTE *buf = ((void *)0);
    {
    }
    {
        (winetest_set_location("encode.c", 2284), 0) ? 0 : winetest_ok(!memcmp(buf, mixedSequence, mixedSequence[1] + 2),
         "Unexpected value\n");
    }
}
static void test_decodeSequenceOfAny(DWORD dwEncoding)
{
    BOOL ret;
    BYTE *buf = ((void *)0);
    DWORD bufSize = 0;
    {
        {
        }
    }
    ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)34), mixedSequence,
     &bufSize);
    {
        CRYPT_SEQUENCE_OF_ANY *seq = (CRYPT_SEQUENCE_OF_ANY *)buf;
        (winetest_set_location("encode.c", 2324), 0) ? 0 : winetest_ok(seq->cValue == sizeof(ints) / sizeof(ints[0]),
         seq->rgValue[0].cbData);
        (winetest_set_location("encode.c", 2330), 0) ? 0 : winetest_ok(!memcmp(seq->rgValue[0].pbData, times[0].encodedTime,
         times[0].encodedTime[1] + 2), "Unexpected value\n");
    }
}
struct encodedExtensions
{
    CERT_EXTENSIONS exts;
};
static BYTE noncrit_ext_data[] = { 0x30,0x06,0x01,0x01,0xff,0x02,0x01,0x01 };
static CHAR oid_basic_constraints2[] = "2.5.29.19";
static CERT_EXTENSION nonCriticalExt =
 { oid_basic_constraints2, 0, { 8, noncrit_ext_data } };
static const struct encodedExtensions exts[] = {
};
static void test_encodeExtensions(DWORD dwEncoding)
{
    DWORD i;
    {
        {
        }
    }
    {
        BOOL ret;
        BYTE *buf = ((void *)0);
        DWORD bufSize = 0;
        ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)5),
         ((void *)0), (BYTE *)&buf, &bufSize);
        {
            CERT_EXTENSIONS *ext = (CERT_EXTENSIONS *)buf;
            DWORD j;
            (winetest_set_location("encode.c", 2405), 0) ? 0 : winetest_ok(ext->cExtension == exts[i].exts.cExtension,
             ext->cExtension);
            {
                (winetest_set_location("encode.c", 2410), 0) ? 0 : winetest_ok(!__extension__ ({ size_t __s1_len, __s2_len; (__builtin_constant_p (ext->rgExtension[j].pszObjId) && __builtin_constant_p (exts[i].exts.rgExtension[j].pszObjId) && (__s1_len = strlen (ext->rgExtension[j].pszObjId), __s2_len = strlen (exts[i].exts.rgExtension[j].pszObjId), (!((size_t)(const void *)((ext->rgExtension[j].pszObjId) + 1) - (size_t)(const void *)(ext->rgExtension[j].pszObjId) == 1) || __s1_len >= 4) && (!((size_t)(const void *)((exts[i].exts.rgExtension[j].pszObjId) + 1) - (size_t)(const void *)(exts[i].exts.rgExtension[j].pszObjId) == 1) || __s2_len >= 4)) ? __builtin_strcmp (ext->rgExtension[j].pszObjId, exts[i].exts.rgExtension[j].pszObjId) : (__builtin_constant_p (ext->rgExtension[j].pszObjId) && ((size_t)(const void *)((ext->rgExtension[j].pszObjId) + 1) - (size_t)(const void *)(ext->rgExtension[j].pszObjId) == 1) && (__s1_len = strlen (ext->rgExtension[j].pszObjId), __s1_len < 4) ? (__builtin_constant_p (exts[i].exts.rgExtension[j].pszObjId) && ((size_t)(const void *)((exts[i].exts.rgExtension[j].pszObjId) + 1) - (size_t)(const void *)(exts[i].exts.rgExtension[j].pszObjId) == 1) ? __builtin_strcmp (ext->rgExtension[j].pszObjId, exts[i].exts.rgExtension[j].pszObjId) : (__extension__ ({ __const unsigned char *__s2 = (__const unsigned char *) (__const char *) (exts[i].exts.rgExtension[j].pszObjId); register int __result = (((__const unsigned char *) (__const char *) (ext->rgExtension[j].pszObjId))[0] - __s2[0]); if (__s1_len > 0 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (ext->rgExtension[j].pszObjId))[1] - __s2[1]); if (__s1_len > 1 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (ext->rgExtension[j].pszObjId))[2] - __s2[2]); if (__s1_len > 2 && __result == 0) __result = (((__const unsigned char *) (__const char *) (ext->rgExtension[j].pszObjId))[3] - __s2[3]); } } __result; }))) : (__builtin_constant_p (exts[i].exts.rgExtension[j].pszObjId) && ((size_t)(const void *)((exts[i].exts.rgExtension[j].pszObjId) + 1) - (size_t)(const void *)(exts[i].exts.rgExtension[j].pszObjId) == 1) && (__s2_len = strlen (exts[i].exts.rgExtension[j].pszObjId), __s2_len < 4) ? (__builtin_constant_p (ext->rgExtension[j].pszObjId) && ((size_t)(const void *)((ext->rgExtension[j].pszObjId) + 1) - (size_t)(const void *)(ext->rgExtension[j].pszObjId) == 1) ? __builtin_strcmp (ext->rgExtension[j].pszObjId, exts[i].exts.rgExtension[j].pszObjId) : (__extension__ ({ __const unsigned char *__s1 = (__const unsigned char *) (__const char *) (ext->rgExtension[j].pszObjId); register int __result = __s1[0] - ((__const unsigned char *) (__const char *) (exts[i].exts.rgExtension[j].pszObjId))[0]; if (__s2_len > 0 && __result == 0) { __result = (__s1[1] - ((__const unsigned char *) (__const char *) (exts[i].exts.rgExtension[j].pszObjId))[1]); if (__s2_len > 1 && __result == 0) { __result = (__s1[2] - ((__const unsigned char *) (__const char *) (exts[i].exts.rgExtension[j].pszObjId))[2]); if (__s2_len > 2 && __result == 0) __result = (__s1[3] - ((__const unsigned char *) (__const char *) (exts[i].exts.rgExtension[j].pszObjId))[3]); } } __result; }))) : __builtin_strcmp (ext->rgExtension[j].pszObjId, exts[i].exts.rgExtension[j].pszObjId)))); }),
                 ext->rgExtension[j].pszObjId);
                (winetest_set_location("encode.c", 2415), 0) ? 0 : winetest_ok(!memcmp(ext->rgExtension[j].Value.pbData,
                 exts[i].exts.rgExtension[j].Value.cbData),
                 "Unexpected value\n");
            }
        }
    }
}
struct encodedPublicKey
{
    const BYTE *encoded;
    const BYTE *encodedNoNull;
};
static const BYTE aKey[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0xa, 0xb, 0xc, 0xd,
 0xe, 0xf };
static const BYTE params[] = { 0x02, 0x01, 0x01 };
static const unsigned char bin66[] = {
    0x30,0x0f,0x30,0x0a,0x06,0x06,0x2a,0x86,0x48,0x86,0xf7,0x0d,0x05,0x00,0x03,0x01,0x00};
static const unsigned char bin67[] = {
    0x30,0x0d,0x30,0x08,0x06,0x06,0x2a,0x86,0x48,0x86,0xf7,0x0d,0x03,0x01,0x00};
static const unsigned char bin69[] = {
    0x0f};
static unsigned char bin72[] = { 0x05,0x00};
static CHAR oid_bogus[] = "1.2.3",
            oid_rsa[] = "1.2.840.113549";
static const struct encodedPublicKey pubKeys[] = {
 { { { oid_rsa, { 0, ((void *)0) } }, { 0, ((void *)0), 0} },
  { { oid_rsa, { 2, bin72 } }, { sizeof(aKey), (BYTE *)aKey, 0} } },
 { { { oid_rsa, { sizeof(params), (BYTE *)params } }, { sizeof(aKey),
  (BYTE *)aKey, 0 } } },
};
static void test_encodePublicKeyInfo(DWORD dwEncoding)
{
    DWORD i;
    {
        BOOL ret;
        BYTE *buf = ((void *)0);
        DWORD bufSize = 0;
        ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)8),
         &bufSize);
        {
            (winetest_set_location("encode.c", 2505), 0) ? 0 : winetest_ok(bufSize == pubKeys[i].encoded[1] + 2 ||
             pubKeys[i].encodedNoNull[1] + 2, bufSize);
                (winetest_set_location("encode.c", 2510), 0) ? 0 : winetest_ok(!memcmp(buf, pubKeys[i].encoded, pubKeys[i].encoded[1] + 2),
                 "Unexpected value\n");
                (winetest_set_location("encode.c", 2513), 0) ? 0 : winetest_ok(!memcmp(buf, pubKeys[i].encodedNoNull,
                 pubKeys[i].encodedNoNull[1] + 2), "Unexpected value\n");
        }
    }
}
static void comparePublicKeyInfo(const CERT_PUBLIC_KEY_INFO *expected,
 const CERT_PUBLIC_KEY_INFO *got)
{
    (winetest_set_location("encode.c", 2523), 0) ? 0 : winetest_ok(!__extension__ ({ size_t __s1_len, __s2_len; (__builtin_constant_p (expected->Algorithm.pszObjId) && __builtin_constant_p (got->Algorithm.pszObjId) && (__s1_len = strlen (expected->Algorithm.pszObjId), __s2_len = strlen (got->Algorithm.pszObjId), (!((size_t)(const void *)((expected->Algorithm.pszObjId) + 1) - (size_t)(const void *)(expected->Algorithm.pszObjId) == 1) || __s1_len >= 4) && (!((size_t)(const void *)((got->Algorithm.pszObjId) + 1) - (size_t)(const void *)(got->Algorithm.pszObjId) == 1) || __s2_len >= 4)) ? __builtin_strcmp (expected->Algorithm.pszObjId, got->Algorithm.pszObjId) : (__builtin_constant_p (expected->Algorithm.pszObjId) && ((size_t)(const void *)((expected->Algorithm.pszObjId) + 1) - (size_t)(const void *)(expected->Algorithm.pszObjId) == 1) && (__s1_len = strlen (expected->Algorithm.pszObjId), __s1_len < 4) ? (__builtin_constant_p (got->Algorithm.pszObjId) && ((size_t)(const void *)((got->Algorithm.pszObjId) + 1) - (size_t)(const void *)(got->Algorithm.pszObjId) == 1) ? __builtin_strcmp (expected->Algorithm.pszObjId, got->Algorithm.pszObjId) : (__extension__ ({ __const unsigned char *__s2 = (__const unsigned char *) (__const char *) (got->Algorithm.pszObjId); register int __result = (((__const unsigned char *) (__const char *) (expected->Algorithm.pszObjId))[0] - __s2[0]); if (__s1_len > 0 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (expected->Algorithm.pszObjId))[1] - __s2[1]); if (__s1_len > 1 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (expected->Algorithm.pszObjId))[2] - __s2[2]); if (__s1_len > 2 && __result == 0) __result = (((__const unsigned char *) (__const char *) (expected->Algorithm.pszObjId))[3] - __s2[3]); } } __result; }))) : (__builtin_constant_p (got->Algorithm.pszObjId) && ((size_t)(const void *)((got->Algorithm.pszObjId) + 1) - (size_t)(const void *)(got->Algorithm.pszObjId) == 1) && (__s2_len = strlen (got->Algorithm.pszObjId), __s2_len < 4) ? (__builtin_constant_p (expected->Algorithm.pszObjId) && ((size_t)(const void *)((expected->Algorithm.pszObjId) + 1) - (size_t)(const void *)(expected->Algorithm.pszObjId) == 1) ? __builtin_strcmp (expected->Algorithm.pszObjId, got->Algorithm.pszObjId) : (__extension__ ({ __const unsigned char *__s1 = (__const unsigned char *) (__const char *) (expected->Algorithm.pszObjId); register int __result = __s1[0] - ((__const unsigned char *) (__const char *) (got->Algorithm.pszObjId))[0]; if (__s2_len > 0 && __result == 0) { __result = (__s1[1] - ((__const unsigned char *) (__const char *) (got->Algorithm.pszObjId))[1]); if (__s2_len > 1 && __result == 0) { __result = (__s1[2] - ((__const unsigned char *) (__const char *) (got->Algorithm.pszObjId))[2]); if (__s2_len > 2 && __result == 0) __result = (__s1[3] - ((__const unsigned char *) (__const char *) (got->Algorithm.pszObjId))[3]); } } __result; }))) : __builtin_strcmp (expected->Algorithm.pszObjId, got->Algorithm.pszObjId)))); }),
     expected->PublicKey.cbData, got->PublicKey.cbData);
        (winetest_set_location("encode.c", 2538), 0) ? 0 : winetest_ok(!memcmp(expected->PublicKey.pbData, got->PublicKey.pbData,
         got->PublicKey.cbData), "Unexpected public key value\n");
}
static void test_decodePublicKeyInfo(DWORD dwEncoding)
{
    static const BYTE bogusPubKeyInfo[] = { 0x30, 0x22, 0x30, 0x0d, 0x06, 0x06,
     0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f };
    BOOL ret;
    BYTE *buf = ((void *)0);
    DWORD bufSize = 0;
    {
        ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)8),
         0x08000, ((void *)0), (BYTE *)&buf, &bufSize);
        {
        }
    }
    ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)8),
     "Expected CRYPT_E_ASN1_CORRUPT, got %08x\n", GetLastError());
}
static const BYTE v1Cert[] = { 0x30, 0x33, 0x02, 0x00, 0x30, 0x02, 0x06, 0x00,
 0x02, 0x06, 0x00, 0x03, 0x01, 0x00 };
static const BYTE v2Cert[] = { 0x30, 0x38, 0xa0, 0x03, 0x02, 0x01, 0x01, 0x02,
 0x30, 0x5a, 0x30, 0x07, 0x30, 0x02, 0x06, 0x00, 0x03, 0x01, 0x00 };
static const BYTE v3Cert[] = { 0x30, 0x38, 0xa0, 0x03, 0x02, 0x01, 0x02, 0x02,
 0x30, 0x5a, 0x30, 0x07, 0x30, 0x02, 0x06, 0x00, 0x03, 0x01, 0x00 };
static const BYTE v1CertWithConstraints[] = { 0x30, 0x4b, 0x02, 0x00, 0x30,
 0x06, 0x01, 0x01, 0xff, 0x02, 0x01, 0x01 };
static const BYTE v1CertWithSerial[] = { 0x30, 0x4c, 0x02, 0x01, 0x01, 0x30,
 0x06, 0x01, 0x01, 0xff, 0x02, 0x01, 0x01 };
static const BYTE bigCert[] = { 0x30, 0x7a, 0x02, 0x01, 0x01, 0x30, 0x02, 0x06,
 0x01, 0xff, 0x04, 0x08, 0x30, 0x06, 0x01, 0x01, 0xff, 0x02, 0x01, 0x01 };
static void test_encodeCertToBeSigned(DWORD dwEncoding)
{
    BOOL ret;
    BYTE *buf = ((void *)0);
    DWORD size = 0;
    CERT_INFO info = { 0 };
    ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)2), ((void *)0),
     0x08000, ((void *)0), (BYTE *)&buf, &size);
    {
    }
    {
    }
    ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)2), &info,
     0x08000, ((void *)0), (BYTE *)&buf, &size);
    {
    }
    {
    }
}
static void test_decodeCertToBeSigned(DWORD dwEncoding)
{
    static const BYTE *corruptCerts[] = { v1Cert, v2Cert, v3Cert,
     v1CertWithConstraints, v1CertWithSerial };
    BOOL ret;
    (winetest_set_location("encode.c", 2727), 0) ? 0 : winetest_ok(!ret && GetLastError() == ((HRESULT)0x80093102L),
     "Expected STATUS_ACCESS_VIOLATION, got %08x\n", GetLastError());
    {
        ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)2),
         "Expected CRYPT_E_ASN1_CORRUPT, got %08x\n", GetLastError());
    }
    {
    }
}
static const BYTE hash[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0xa, 0xb, 0xc, 0xd,
 0xe, 0xf };
static const BYTE signedBigCert[] = {
 0x06, 0x05, 0x04, 0x03, 0x02, 0x01, 0x00 };
static void test_encodeCert(DWORD dwEncoding)
{
    CERT_SIGNED_CONTENT_INFO info = { { sizeof(bigCert), (BYTE *)bigCert },
     { ((void *)0), { 0, ((void *)0) } }, { sizeof(hash), (BYTE *)hash, 0 } };
    BOOL ret;
    BYTE *buf = ((void *)0);
    DWORD bufSize = 0;
    ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)1), &info,
     0x08000, ((void *)0), (BYTE *)&buf, &bufSize);
    {
    }
}
static void test_decodeCert(DWORD dwEncoding)
{
    BOOL ret;
    BYTE *buf = ((void *)0);
    DWORD size = 0;
    {
    }
    ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)2), signedBigCert,
     sizeof(signedBigCert), 0x08000, ((void *)0), (BYTE *)&buf, &size);
    {
        CERT_INFO *info = (CERT_INFO *)buf;
        (winetest_set_location("encode.c", 2843), 0) ? 0 : winetest_ok(info->SerialNumber.cbData == 1,
         *info->SerialNumber.pbData);
        (winetest_set_location("encode.c", 2852), 0) ? 0 : winetest_ok(info->Subject.cbData == sizeof(encodedCommonName),
         "Wrong size %d\n", info->Subject.cbData);
        (winetest_set_location("encode.c", 2854), 0) ? 0 : winetest_ok(!memcmp(info->Subject.pbData, encodedCommonName,
         info->Subject.cbData), "Unexpected subject\n");
    }
}
static const BYTE distPointWithUrl[] = { 0x30, 0x19, 0x30, 0x17, 0xa0, 0x15,
 0x6e, 0x65, 0x68, 0x71, 0x2e, 0x6f, 0x72, 0x67 };
static const BYTE distPointWithIssuer[] = { 0x30, 0x17, 0x30, 0x15, 0xa2, 0x13,
 0x2e, 0x6f, 0x72, 0x67 };
static const BYTE crlReason = 1 |
 3;
static void test_encodeCRLDistPoints(DWORD dwEncoding)
{
    CRL_DIST_POINTS_INFO xxxinfo = { 0 };
    CRL_DIST_POINT point = { { 0 } };
    BOOL ret;
    BYTE *buf = ((void *)0);
    DWORD size = 0;
    ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)35), &xxxinfo,
     "Expected E_INVALIDARG, got %08x\n", GetLastError());
    xxxinfo.cDistPoint = 1;
    xxxinfo.rgDistPoint = &point;
    ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)35), &xxxinfo,
     0x08000, ((void *)0), (BYTE *)&buf, &size);
    point.ReasonFlags.cbData = sizeof(crlReason);
    {
    }
}
static void test_decodeCRLDistPoints(DWORD dwEncoding)
{
    BOOL ret;
    BYTE *buf = ((void *)0);
    DWORD size = 0;
    PCERT_ALT_NAME_ENTRY entry;
    ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)35),
     (BYTE *)&buf, &size);
    {
        (winetest_set_location("encode.c", 3065), 0) ? 0 : winetest_ok(entry->dwAltNameChoice == 7,
         "Expected CERT_ALT_NAME_URL, got %d\n", entry->dwAltNameChoice);
    }
}
static const BYTE urlIDP[] = { 0x30,0x17,0xa0,0x15,0xa0,0x13,0x86,0x11,0x68,
 0x67 };
static void test_encodeCRLIssuingDistPoint(DWORD dwEncoding)
{
    BOOL ret;
    BYTE *buf = ((void *)0);
    DWORD size = 0;
    CRL_ISSUING_DIST_POINT point = { { 0 } };
    ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)54), ((void *)0),
     "Expected STATUS_ACCESS_VIOLATION, got %08x\n", GetLastError());
    {
    }
    ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)54), &point,
     0x08000, ((void *)0), (BYTE *)&buf, &size);
    {
    }
    {
    }
}
static void compareAltNameEntry(const CERT_ALT_NAME_ENTRY *expected,
 const CERT_ALT_NAME_ENTRY *got)
{
    (winetest_set_location("encode.c", 3149), 0) ? 0 : winetest_ok(expected->dwAltNameChoice == got->dwAltNameChoice,
     got->dwAltNameChoice);
    {
        {
            (winetest_set_location("encode.c", 3161), 0) ? 0 : winetest_ok((!(*expected).pwszURL && !(*got).pwszURL) ||
               !lstrcmpW((*expected).pwszURL, (*got).pwszURL), "Unexpected name\n");
        }
    }
}
static void compareAltNameInfo(const CERT_ALT_NAME_INFO *expected,
 const CERT_ALT_NAME_INFO *got)
{
}
static const BYTE v1CRL[] = { 0x30, 0x15, 0x30, 0x02, 0x06, 0x00, 0x18, 0x0f,
 0x30, 0x5a };
static const BYTE v2CRL[] = { 0x30, 0x18, 0x02, 0x01, 0x01, 0x30, 0x02, 0x06,
 0x30, 0x30, 0x30, 0x30, 0x5a };
static const BYTE v1CRLWithIssuer[] = { 0x30, 0x2c, 0x30, 0x02, 0x06, 0x00,
 0x5a };
static const BYTE v1CRLWithIssuerAndEmptyEntry[] = { 0x30, 0x43, 0x30, 0x02,
 0x30, 0x31, 0x30, 0x31, 0x30, 0x31, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x5a };
static const BYTE v1CRLWithIssuerAndEntry[] = { 0x30, 0x44, 0x30, 0x02, 0x06,
 0x30, 0x31, 0x30, 0x31, 0x30, 0x31, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x5a };
static const BYTE v1CRLWithEntryExt[] = { 0x30,0x5a,0x30,0x02,0x06,0x00,0x30,
 0x04,0x08,0x30,0x06,0x01,0x01,0xff,0x02,0x01,0x01 };
static const BYTE v1CRLWithExt[] = { 0x30,0x5c,0x30,0x02,0x06,0x00,0x30,0x15,
 0xff,0x04,0x08,0x30,0x06,0x01,0x01,0xff,0x02,0x01,0x01 };
static const BYTE v2CRLWithExt[] = { 0x30,0x5c,0x02,0x01,0x01,0x30,0x02,0x06,
 0x13,0x04,0x08,0x30,0x06,0x01,0x01,0xff,0x02,0x01,0x01 };
static const BYTE v2CRLWithIssuingDistPoint[] = { 0x30,0x5c,0x02,0x01,0x01,
 0x03,0x55,0x1d,0x13,0x04,0x08,0x30,0x06,0x01,0x01,0xff,0x02,0x01,0x01 };
static void test_encodeCRLToBeSigned(DWORD dwEncoding)
{
    BOOL ret;
    BYTE *buf = ((void *)0);
    DWORD size = 0;
    CRL_INFO info = { 0 };
    {
        (winetest_set_location("encode.c", 3386), 0) ? 0 : winetest_ok(size == sizeof(v1CRLWithIssuerAndEntry),
         "Wrong size %d\n", size);
    }
    {
    }
    ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)3), &info,
     0x08000, ((void *)0), (BYTE *)&buf, &size);
    {
    }
    {
    }
}
static const BYTE verisignCRL[] = { 0x30, 0x82, 0x01, 0xb1, 0x30, 0x82, 0x01,
0x61,0xc0,0x99,0x16,0x71,0x05,0xb6,0x25,0x14,0x64,0x4f,0x30 };
static void test_decodeCRLToBeSigned(DWORD dwEncoding)
{
    BOOL ret;
    BYTE *buf = ((void *)0);
    DWORD size = 0, i;
    {
    }
    ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)3),
     (BYTE *)&buf, &size);
    {
        CRL_INFO *info = (CRL_INFO *)buf;
        (winetest_set_location("encode.c", 4016), 0) ? 0 : winetest_ok(info->cCRLEntry == 0, "Expected 0 CRL entries, got %d\n",
         "Unexpected issuer\n");
    }
    ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)3),
     0x08000, ((void *)0), (BYTE *)&buf, &size);
    {
        CRL_INFO *info = (CRL_INFO *)buf;
        (winetest_set_location("encode.c", 4041), 0) ? 0 : winetest_ok(info->cCRLEntry == 1, "Expected 1 CRL entries, got %d\n",
         "Unexpected issuer\n");
    }
    ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)3),
     0x08000, ((void *)0), (BYTE *)&buf, &size);
    {
        CRL_INFO *info = (CRL_INFO *)buf;
        (winetest_set_location("encode.c", 4085), 0) ? 0 : winetest_ok(info->cCRLEntry == 209, "Expected 209 CRL entries, got %d\n",
         info->cExtension);
    }
    ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)3),
     ((void *)0), (BYTE *)&buf, &size);
    {
    }
}
static const BYTE authorityKeyIdWithIssuer[] = { 0x30,0x19,0xa1,0x17,0x30,0x15,
 0x20,0x4c,0x61,0x6e,0x67,0x00 };
static const BYTE authorityKeyIdWithSerial[] = { 0x30,0x03,0x82,0x01,0x01 };
static void test_encodeAuthorityKeyId(DWORD dwEncoding)
{
    CERT_AUTHORITY_KEY_ID_INFO info = { { 0 } };
    BOOL ret;
    BYTE *buf = ((void *)0);
    DWORD size = 0;
    {
    }
    {
    }
    ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)9), &info,
     0x08000, ((void *)0), (BYTE *)&buf, &size);
    {
    }
    ret = CryptEncodeObjectEx(dwEncoding, ((LPCSTR)9), &info,
     0x08000, ((void *)0), (BYTE *)&buf, &size);
    {
        (winetest_set_location("encode.c", 4284), 0) ? 0 : winetest_ok(size == sizeof(authorityKeyIdWithSerial), "Unexpected size %d\n",
         size);
    }
}
static void test_decodeAuthorityKeyId(DWORD dwEncoding)
{
    BOOL ret;
    LPBYTE buf = ((void *)0);
    DWORD size = 0;
    ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)9),
     (BYTE *)&buf, &size);
    {
    }
    ret = CryptDecodeObjectEx(dwEncoding, ((LPCSTR)9),
     0x08000, ((void *)0), (BYTE *)&buf, &size);
    {
        (winetest_set_location("encode.c", 4355), 0) ? 0 : winetest_ok(size >= sizeof(CERT_AUTHORITY_KEY_ID_INFO), "Unexpected size %d\n",
         "Unexpected serial number\n");
    }
}
static void testExportPublicKey(HCRYPTPROV csp, PCERT_PUBLIC_KEY_INFO *pInfo)
{
    BOOL ret;
    DWORD size = 0;
    {
        ret = CryptExportPublicKeyInfoEx(csp, 2, 0x00000001,
         ((void *)0), 0, ((void *)0), ((void *)0), &size);
        {
            {
                (winetest_set_location("encode.c", 4416), 0) ? 0 : winetest_ok(!__extension__ ({ size_t __s1_len, __s2_len; (__builtin_constant_p ((*pInfo)->Algorithm.pszObjId) && __builtin_constant_p ("1.2.840.113549.1.1.1") && (__s1_len = strlen ((*pInfo)->Algorithm.pszObjId), __s2_len = strlen ("1.2.840.113549.1.1.1"), (!((size_t)(const void *)(((*pInfo)->Algorithm.pszObjId) + 1) - (size_t)(const void *)((*pInfo)->Algorithm.pszObjId) == 1) || __s1_len >= 4) && (!((size_t)(const void *)(("1.2.840.113549.1.1.1") + 1) - (size_t)(const void *)("1.2.840.113549.1.1.1") == 1) || __s2_len >= 4)) ? __builtin_strcmp ((*pInfo)->Algorithm.pszObjId, "1.2.840.113549.1.1.1") : (__builtin_constant_p ((*pInfo)->Algorithm.pszObjId) && ((size_t)(const void *)(((*pInfo)->Algorithm.pszObjId) + 1) - (size_t)(const void *)((*pInfo)->Algorithm.pszObjId) == 1) && (__s1_len = strlen ((*pInfo)->Algorithm.pszObjId), __s1_len < 4) ? (__builtin_constant_p ("1.2.840.113549.1.1.1") && ((size_t)(const void *)(("1.2.840.113549.1.1.1") + 1) - (size_t)(const void *)("1.2.840.113549.1.1.1") == 1) ? __builtin_strcmp ((*pInfo)->Algorithm.pszObjId, "1.2.840.113549.1.1.1") : (__extension__ ({ __const unsigned char *__s2 = (__const unsigned char *) (__const char *) ("1.2.840.113549.1.1.1"); register int __result = (((__const unsigned char *) (__const char *) ((*pInfo)->Algorithm.pszObjId))[0] - __s2[0]); if (__s1_len > 0 && __result == 0) { __result = (((__const unsigned char *) (__const char *) ((*pInfo)->Algorithm.pszObjId))[1] - __s2[1]); if (__s1_len > 1 && __result == 0) { __result = (((__const unsigned char *) (__const char *) ((*pInfo)->Algorithm.pszObjId))[2] - __s2[2]); if (__s1_len > 2 && __result == 0) __result = (((__const unsigned char *) (__const char *) ((*pInfo)->Algorithm.pszObjId))[3] - __s2[3]); } } __result; }))) : (__builtin_constant_p ("1.2.840.113549.1.1.1") && ((size_t)(const void *)(("1.2.840.113549.1.1.1") + 1) - (size_t)(const void *)("1.2.840.113549.1.1.1") == 1) && (__s2_len = strlen ("1.2.840.113549.1.1.1"), __s2_len < 4) ? (__builtin_constant_p ((*pInfo)->Algorithm.pszObjId) && ((size_t)(const void *)(((*pInfo)->Algorithm.pszObjId) + 1) - (size_t)(const void *)((*pInfo)->Algorithm.pszObjId) == 1) ? __builtin_strcmp ((*pInfo)->Algorithm.pszObjId, "1.2.840.113549.1.1.1") : (__extension__ ({ __const unsigned char *__s1 = (__const unsigned char *) (__const char *) ((*pInfo)->Algorithm.pszObjId); register int __result = __s1[0] - ((__const unsigned char *) (__const char *) ("1.2.840.113549.1.1.1"))[0]; if (__s2_len > 0 && __result == 0) { __result = (__s1[1] - ((__const unsigned char *) (__const char *) ("1.2.840.113549.1.1.1"))[1]); if (__s2_len > 1 && __result == 0) { __result = (__s1[2] - ((__const unsigned char *) (__const char *) ("1.2.840.113549.1.1.1"))[2]); if (__s2_len > 2 && __result == 0) __result = (__s1[3] - ((__const unsigned char *) (__const char *) ("1.2.840.113549.1.1.1"))[3]); } } __result; }))) : __builtin_strcmp ((*pInfo)->Algorithm.pszObjId, "1.2.840.113549.1.1.1")))); }),
                 (*pInfo)->Algorithm.pszObjId);
            }
        }
    }
}
static const BYTE expiredCert[] = { 0x30, 0x82, 0x01, 0x33, 0x30, 0x81, 0xe2,
 0x49, 0xe5, 0xf9, 0x65, 0xf3 };
static void testImportPublicKey(HCRYPTPROV csp, PCERT_PUBLIC_KEY_INFO info)
{
    BOOL ret;
    HCRYPTKEY key;
    PCCERT_CONTEXT context;
    (winetest_set_location("encode.c", 4464), 0) ? 0 : winetest_ok(!ret && GetLastError() == 2,
     GetLastError());
    {
        (winetest_set_location("encode.c", 4485), 0) ? 0 : winetest_ok(!__extension__ ({ size_t __s1_len, __s2_len; (__builtin_constant_p ("1.2.840.113549.1.1.1") && __builtin_constant_p (context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId) && (__s1_len = strlen ("1.2.840.113549.1.1.1"), __s2_len = strlen (context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId), (!((size_t)(const void *)(("1.2.840.113549.1.1.1") + 1) - (size_t)(const void *)("1.2.840.113549.1.1.1") == 1) || __s1_len >= 4) && (!((size_t)(const void *)((context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId) + 1) - (size_t)(const void *)(context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId) == 1) || __s2_len >= 4)) ? __builtin_strcmp ("1.2.840.113549.1.1.1", context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId) : (__builtin_constant_p ("1.2.840.113549.1.1.1") && ((size_t)(const void *)(("1.2.840.113549.1.1.1") + 1) - (size_t)(const void *)("1.2.840.113549.1.1.1") == 1) && (__s1_len = strlen ("1.2.840.113549.1.1.1"), __s1_len < 4) ? (__builtin_constant_p (context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId) && ((size_t)(const void *)((context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId) + 1) - (size_t)(const void *)(context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId) == 1) ? __builtin_strcmp ("1.2.840.113549.1.1.1", context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId) : (__extension__ ({ __const unsigned char *__s2 = (__const unsigned char *) (__const char *) (context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId); register int __result = (((__const unsigned char *) (__const char *) ("1.2.840.113549.1.1.1"))[0] - __s2[0]); if (__s1_len > 0 && __result == 0) { __result = (((__const unsigned char *) (__const char *) ("1.2.840.113549.1.1.1"))[1] - __s2[1]); if (__s1_len > 1 && __result == 0) { __result = (((__const unsigned char *) (__const char *) ("1.2.840.113549.1.1.1"))[2] - __s2[2]); if (__s1_len > 2 && __result == 0) __result = (((__const unsigned char *) (__const char *) ("1.2.840.113549.1.1.1"))[3] - __s2[3]); } } __result; }))) : (__builtin_constant_p (context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId) && ((size_t)(const void *)((context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId) + 1) - (size_t)(const void *)(context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId) == 1) && (__s2_len = strlen (context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId), __s2_len < 4) ? (__builtin_constant_p ("1.2.840.113549.1.1.1") && ((size_t)(const void *)(("1.2.840.113549.1.1.1") + 1) - (size_t)(const void *)("1.2.840.113549.1.1.1") == 1) ? __builtin_strcmp ("1.2.840.113549.1.1.1", context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId) : (__extension__ ({ __const unsigned char *__s1 = (__const unsigned char *) (__const char *) ("1.2.840.113549.1.1.1"); register int __result = __s1[0] - ((__const unsigned char *) (__const char *) (context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId))[0]; if (__s2_len > 0 && __result == 0) { __result = (__s1[1] - ((__const unsigned char *) (__const char *) (context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId))[1]); if (__s2_len > 1 && __result == 0) { __result = (__s1[2] - ((__const unsigned char *) (__const char *) (context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId))[2]); if (__s2_len > 2 && __result == 0) __result = (__s1[3] - ((__const unsigned char *) (__const char *) (context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId))[3]); } } __result; }))) : __builtin_strcmp ("1.2.840.113549.1.1.1", context->pCertInfo->SubjectPublicKeyInfo.Algorithm.pszObjId)))); }),
         &context->pCertInfo->SubjectPublicKeyInfo, 0, 0, ((void *)0), &key);
    }
}
static const char cspName[] = "WineCryptTemp";
static void testPortPublicKeyInfo(void)
{
    HCRYPTPROV csp;
    BOOL ret;
    CryptAcquireContextA(&csp, cspName, "Microsoft Base Cryptographic Provider v1.0", 1,
     0x00000008);
    ret = CryptAcquireContextA(&csp, cspName, "Microsoft Base Cryptographic Provider v1.0", 1,
     0x00000010);
}
void func_encode(void)
{
    test_encodeCRLDistPoints(0x00000001);
}

/* { dg-final { scan-tree-dump-times " xxxinfo = {}" 1 "dse1"} } */
/* { dg-final { cleanup-tree-dump "dse\[1-2\]" } } */
