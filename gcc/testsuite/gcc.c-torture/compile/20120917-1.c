typedef long long curl_off_t;
int tool_seek_cb(void *userdata, curl_off_t offset, int whence)
{
  if(offset > 0x7FFFFFFFLL - 0x1LL) 
{
    curl_off_t left = offset;
    while(left) 
{
      long step = (left > 0x7FFFFFFFLL - 0x1LL) ? 2147483647L - 1L : (long)left;
      left -= step;
    }
  }
}
