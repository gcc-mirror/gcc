/* { dg-do compile } */
/* { dg-options "-w -g -Os -march=i386 -mregparm=3 -m32 -fno-PIE" } */

typedef unsigned char uint8_t;
typedef unsigned int uint32_t;
typedef unsigned int size_t;
typedef uint32_t bigint_element_t;

/**
 * Define a big-integer type
 *
 * @v size		Number of elements
 * @ret bigint_t	Big integer type
 */
 #define bigint_t( size )						\
 struct {							\
      bigint_element_t element[ (size) ];			\
 }

/**
* Determine number of elements required for a big-integer type
*
* @v len		Maximum length of big integer, in bytes
* @ret size		Number of elements
*/
#define bigint_required_size( len )					\
 ( ( (len) + sizeof ( bigint_element_t ) - 1 ) /			\
   sizeof ( bigint_element_t ) )

/**
 * Determine number of elements in big-integer type
 *
 * @v bigint		Big integer
 * @ret size		Number of elements
 */
 #define bigint_size( bigint )						\
 ( sizeof ( *(bigint) ) / sizeof ( (bigint)->element[0] ) )

 /**
 * Initialise big integer
 *
 * @v value		Big integer to initialise
 * @v data		Raw data
 * @v len		Length of raw data
 */
#define bigint_init( value, data, len ) do {				\
	unsigned int size = bigint_size (value);			\
	bigint_init_raw ( (value)->element, size, (data), (len) );	\
	} while ( 0 )


/**
 * Calculate temporary working space required for moduluar exponentiation
 *
 * @v modulus		Big integer modulus
 * @ret len		Length of temporary working space
 */
 #define bigint_mod_exp_tmp_len( modulus ) ( {				\
	unsigned int size = bigint_size (modulus);			\
	sizeof ( struct {						\
		bigint_t ( size ) temp[4];				\
	} ); } )


/**
 * Initialise big integer
 *
 * @v value0		Element 0 of big integer to initialise
 * @v size		Number of elements
 * @v data		Raw data
 * @v len		Length of raw data
 */
 static inline __attribute__ (( always_inline )) void
 bigint_init_raw ( uint32_t *value0, unsigned int size,
             const void *data, size_t len ) {
      bigint_t ( size ) __attribute__ (( may_alias )) *value =
           ( ( void * ) value0 );
      long pad_len = ( sizeof ( *value ) - len );
      void *discard_D;
      long discard_c;

      /* Copy raw data in reverse order, padding with zeros */
      __asm__ __volatile__ ( "\n1:\n\t"
                       "movb -1(%3,%1), %%al\n\t"
                       "stosb\n\t"
                       "loop 1b\n\t"
                       "xorl %%eax, %%eax\n\t"
                       "mov %4, %1\n\t"
                       "rep stosb\n\t"
                       : "=&D" ( discard_D ), "=&c" ( discard_c ),
                      "+m" ( *value )
                       : "r" ( data ), "g" ( pad_len ), "0" ( value0 ),
                      "1" ( len )
                       : "eax" );
 }

extern void touch (void *, ...);
extern void touch3 (void *, void *, void *);
extern void touch2 (void *, void *);

/**
 * Perform big integer self-tests
 *
 */
void bigint_test_exec ( void ) {
    do{
	static const uint8_t base_raw[3] = {0};
	static const uint8_t modulus_raw[3] = {0};
	static const uint8_t exponent_raw[25] = {0};
	unsigned int size =
		bigint_required_size ( sizeof ( base_raw ) );
	unsigned int exponent_size =
		bigint_required_size ( sizeof ( exponent_raw ) );
	bigint_t ( size ) base_temp;
	bigint_t ( size ) modulus_temp;
	bigint_t ( exponent_size ) exponent_temp;
	size_t tmp_len = bigint_mod_exp_tmp_len ( &modulus_temp );


	touch ( &base_temp );
	bigint_init ( &modulus_temp, modulus_raw,
		      sizeof ( modulus_raw ) );
	bigint_init ( &exponent_temp, exponent_raw,
		      sizeof ( exponent_raw ) );
	touch3 ( &base_temp, &modulus_temp, &exponent_temp );
	} while ( 0 );
}
