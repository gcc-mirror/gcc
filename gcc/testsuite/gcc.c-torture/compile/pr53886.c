/* PR target/53886  */
typedef struct asn1_string_st ASN1_BIT_STRING;
typedef struct bignum_st BIGNUM;
typedef struct ec_group_st EC_GROUP;
typedef struct ec_key_st EC_KEY;

struct ec_key_st
{
  EC_GROUP *group;
  BIGNUM *priv_key;
  unsigned int enc_flag;
}
X9_62_PENTANOMIAL;
typedef struct ec_privatekey_st
{
  ASN1_BIT_STRING *publicKey;
}
EC_PRIVATEKEY;

extern EC_PRIVATEKEY* EC_PRIVATEKEY_new (void);
extern void EC_PRIVATEKEY_free (EC_PRIVATEKEY*);
extern unsigned char* CRYPTO_realloc (char*,int,const char*,int);

int
i2d_ECPrivateKey (EC_KEY * a, unsigned char **out)
{
  int ret = 0, ok = 0;
  unsigned char *buffer = 0;
  unsigned buf_len = 0, tmp_len;
  EC_PRIVATEKEY *priv_key = 0;
  if (a == 0 || a->group == 0 || a->priv_key == 0)
    {
      ERR_put_error (16, (192), ((3 | 64)),
		     "",
		     1234);
      goto err;
    }
  if ((priv_key = EC_PRIVATEKEY_new ()) == 0)
    {
      ERR_put_error (16, (192), ((1 | 64)),
		     "",
		     1241);
      goto err;
    }
  if (!(a->enc_flag & 0x002))
    {
      if (priv_key->publicKey == 0)
	{
	  goto err;
	}
      if (tmp_len > buf_len)
	{
	  unsigned char *tmp_buffer =
	    CRYPTO_realloc ((char *) buffer, (int) tmp_len, "", 1293);
	  buffer = tmp_buffer;
	}
    }
  if ((ret = i2d_EC_PRIVATEKEY (priv_key, out)) == 0)
    {
    }
  ok = 1;
err:
  if (buffer)
    CRYPTO_free (buffer);
  if (priv_key)
    EC_PRIVATEKEY_free (priv_key);
  return (ok ? ret : 0);
}
