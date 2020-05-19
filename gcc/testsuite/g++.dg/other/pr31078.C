typedef int SLONG __attribute__((mode (__SI__)));

typedef char SCHAR;
typedef short SSHORT;
typedef char TEXT;
typedef long ISC_STATUS;
const SLONG gds_arg_string = 2;
const SLONG gds_sys_request = 335544373L;
enum jrd_blk_t
{
    type_str, type_dcc, type_sbm, type_smb, type_blb, type_irb, type_jrn
};
struct blk
{
};
template < class RPT, SSHORT BLOCK_TYPE = 0 > class pool_alloc_rpt:public blk
{
};
class jrn:public pool_alloc_rpt < SCHAR, type_jrn >
{
public:ISC_STATUS * jrn_status_vector;
  TEXT jrn_server[1];
};
typedef jrn *JRN;
extern void IBERR_build_status (ISC_STATUS *, ISC_STATUS, ...);
static void
error (ISC_STATUS * status_vector, JRN journal, int status, TEXT * string)
{
  IBERR_build_status (status_vector, gds_sys_request, gds_arg_string, string,
		      gds_arg_string, (journal) ? journal->jrn_server : "",
		      0);
}
