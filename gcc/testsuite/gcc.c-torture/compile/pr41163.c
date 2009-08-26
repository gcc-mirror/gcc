struct option {
    void *value;
};
void parse_options (struct option *);
void cmd_grep(void)
{
  struct option options[] = { { &options } };
  parse_options(options);
}

