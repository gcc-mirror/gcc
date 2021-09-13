/* { dg-do compile } */

unsigned int
strlenx(char *s)
{
  char *orig_s = s;
  for (; *s; ++s)
    ;
  return s - orig_s;
}

struct i2c_adapter {
    char name[48];
};

struct {
    int instance;
    struct i2c_adapter i2c_adap[];
} * init_cx18_i2c_cx;

const struct i2c_adapter cx18_i2c_adap_template = {""};
int init_cx18_i2c___trans_tmp_1;

void
init_cx18_i2c()
{
  int i = 0;
  for (;; i++) {
      init_cx18_i2c_cx->i2c_adap[i] = cx18_i2c_adap_template;
      init_cx18_i2c___trans_tmp_1
	= strlenx(init_cx18_i2c_cx->i2c_adap[i].name);
  }
}
