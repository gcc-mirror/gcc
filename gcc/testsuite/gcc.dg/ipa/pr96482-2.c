/* PR ipa/96482 */
/* { dg-do compile } */
/* { dg-options "-std=gnu17 -O2"  } */

int i2c_transfer();
void _dev_err();

struct i2c_msg {
  char bufaddr;
  int adapterdev;
} wdt87xx_i2c_xfer_client;

int wdt87xx_i2c_xfer_client_0, wdt87xx_i2c_xfer_rxdata, wdt87xx_get_string_str_idx;

void
static wdt87xx_i2c_xfer(void *txdata, unsigned rxlen) {
  struct i2c_msg msgs[] = {wdt87xx_i2c_xfer_client_0, rxlen,
                           wdt87xx_i2c_xfer_rxdata};
  int error = i2c_transfer(wdt87xx_i2c_xfer_client, msgs);
  _dev_err("", __func__, error);
}
static void wdt87xx_get_string(unsigned len) {
  char tx_buf[] = {wdt87xx_get_string_str_idx, 3};
  int rx_len = len + 2;
  wdt87xx_i2c_xfer(tx_buf, rx_len);
}

void
wdt87xx_ts_probe_tx_buf() {
  wdt87xx_get_string(34);
  wdt87xx_get_string(8);
  wdt87xx_i2c_xfer(wdt87xx_ts_probe_tx_buf, 2);
}
